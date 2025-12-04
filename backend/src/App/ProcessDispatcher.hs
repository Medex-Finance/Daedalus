{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module App.ProcessDispatcher
  ( ProcessOutput(..)
  , runCommandInDir
  , launchServiceInDir
  ) where

import App.Direnv (wrapWithDirenv)
import App.Foundation (App(..))
import App.Logging (logError, logInfo, logWarn)
import App.Models (TaskId)
import App.StatusStream (StatusEnvelope(..), publishStatus)
import App.Types (AgentRole, StatusEventDTO(..), WorkflowStep)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (Async, async, wait)
import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Exception (SomeException, catch, displayException)
import Control.Monad (unless, void)
import Data.Aeson (object, (.=))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.IO
  ( BufferMode(LineBuffering)
  , Handle
  , hClose
  , hIsEOF
  , hSetBuffering
  )
import System.IO.Error (isEOFError)
import System.Process
  ( CreateProcess(..)
  , ProcessHandle
  , StdStream(CreatePipe)
  , createProcess
  , proc
  , waitForProcess
  , withCreateProcess
  )

-- | Captured result from a dispatched command.
data ProcessOutput = ProcessOutput
  { poExitCode :: ExitCode
  , poStdout :: Text
  , poStderr :: Text
  }

-- | Run a command to completion inside the provided directory, streaming output to the live status hub.
runCommandInDir
  :: App
  -> TaskId
  -> WorkflowStep
  -> AgentRole
  -> Text -- ^ label describing the command (e.g. "tests")
  -> FilePath
  -> Text -- ^ shell command to execute
  -> IO ProcessOutput
runCommandInDir App { appStatusHub, appHeartbeatVar } taskId step role label workingDir commandText = do
  env <- getEnvironment
  ((execCmd, execArgs, finalEnv), direnvNote) <- wrapWithDirenv (Just workingDir) "bash" ["-lc", T.unpack commandText] env
  for_ direnvNote logWarn
  stdoutRef <- newIORef []
  stderrRef <- newIORef []
  logInfo $ "Dispatching command " <> label <> " for task " <> T.pack (show taskId)
  let processSpec =
        (proc execCmd execArgs)
          { cwd = Just workingDir
          , env = Just finalEnv
          , std_out = CreatePipe
          , std_err = CreatePipe
          }
  result <- withCreateProcess processSpec $ \_ mOut mErr ph -> do
    stdoutAsync <- traverse (async . pumpStream "stdout" stdoutRef) mOut
    stderrAsync <- traverse (async . pumpStream "stderr" stderrRef) mErr
    exitCode <- waitForProcess ph
    mapM_ waitMaybe stdoutAsync
    mapM_ waitMaybe stderrAsync
    pure exitCode
    `catch` \(err :: SomeException) -> do
      let errMsg = T.pack (displayException err)
      logError $ "Command " <> label <> " failed to start: " <> errMsg
      pure (ExitFailure 127)
  stdoutTxt <- packRef stdoutRef
  stderrTxt <- packRef stderrRef
  pure ProcessOutput
    { poExitCode = result
    , poStdout = stdoutTxt
    , poStderr = stderrTxt
    }
  where
    pumpStream :: Text -> IORef [Text] -> Handle -> IO ()
    pumpStream streamName ref handle = do
      hSetBuffering handle LineBuffering
      let loop = do
            eof <- hIsEOF handle
            unless eof $ do
              lineEither <- catch (fmap Right (TIO.hGetLine handle)) $ \e ->
                if isEOFError e then pure (Left ()) else pure (Left ())
              case lineEither of
                Left _ -> pure ()
                Right line -> do
                  let decorated = decorateLine streamName line
                  publish decorated streamName
                  modifyIORef' ref (line :)
                  loop
      loop
      hClose handle

    publish line streamName = do
      now <- getCurrentTime
      atomically $ modifyTVar' appHeartbeatVar (Map.insert taskId now)
      publishStatus appStatusHub StatusEnvelope
        { envelopeTaskId = taskId
        , envelopeEvent = StatusEventDTO
            { statusEventId = Nothing
            , statusEventStep = step
            , statusEventMessage = line
            , statusEventCreatedAt = now
            , statusEventPayload = Just $ object
                [ "kind" .= ("agent-log" :: Text)
                , "stream" .= streamName
                , "line" .= line
                , "role" .= role
                , "command" .= label
                ]
            }
        }

    decorateLine streamName line =
      "[" <> label <> "][" <> streamName <> "] " <> line

    packRef ref = T.intercalate "\n" . reverse <$> readIORef ref

    waitMaybe :: Async a -> IO a
    waitMaybe = wait

-- | Launch a long-running service command, streaming its output and returning a handle for later shutdown.
launchServiceInDir
  :: App
  -> TaskId
  -> WorkflowStep
  -> AgentRole
  -> Text -- ^ label
  -> FilePath -- ^ working directory
  -> FilePath -- ^ path to append log output
  -> [(String, String)] -- ^ environment overrides
  -> Text -- ^ command
  -> IO (Either Text ProcessHandle)
launchServiceInDir App { appStatusHub, appHeartbeatVar } taskId step role label workingDir logPath envExtras commandText = do
  env <- getEnvironment
  logInfo $ "Starting service " <> label <> " for task " <> T.pack (show taskId)
  TIO.appendFile logPath $ "\n=== " <> label <> " launch ===\n"
  let envWithExtras = mergeEnv envExtras env
  ((execCmd, execArgs, finalEnv), direnvNote) <- wrapWithDirenv (Just workingDir) "bash" ["-lc", T.unpack commandText] envWithExtras
  for_ direnvNote logWarn
  let processSpec =
        (proc execCmd execArgs)
          { cwd = Just workingDir
          , env = Just finalEnv
          , std_out = CreatePipe
          , std_err = CreatePipe
          }
  result <- (do
      (mIn, mOut, mErr, ph) <- createProcess processSpec
      maybe (pure ()) hClose mIn
      mapM_ forkPump [("stdout", mOut), ("stderr", mErr)]
      pure (Right ph)
    ) `catch` \(err :: SomeException) -> do
      let errMsg = T.pack (displayException err)
      logError $ "Service " <> label <> " failed to start: " <> errMsg
      pure (Left errMsg)
  pure result
  where
    forkPump (streamName, mHandle) =
      case mHandle of
        Nothing -> pure ()
        Just handle ->
          void $ forkIO $ do
            hSetBuffering handle LineBuffering
            let loop = do
                  eof <- hIsEOF handle
                  unless eof $ do
                    lineEither <- catch (fmap Right (TIO.hGetLine handle)) $ \e ->
                      if isEOFError e then pure (Left ()) else pure (Left ())
                    case lineEither of
                      Left _ -> pure ()
                      Right line -> do
                        let decorated = decorateLine streamName line
                        TIO.appendFile logPath (decorated <> "\n")
                        publishLog decorated streamName
                        loop
            loop
            hClose handle
    publishLog line streamName = do
      now <- getCurrentTime
      atomically $ modifyTVar' appHeartbeatVar (Map.insert taskId now)
      publishStatus appStatusHub StatusEnvelope
        { envelopeTaskId = taskId
        , envelopeEvent = StatusEventDTO
            { statusEventId = Nothing
            , statusEventStep = step
            , statusEventMessage = line
            , statusEventCreatedAt = now
            , statusEventPayload = Just $ object
                [ "kind" .= ("agent-log" :: Text)
                , "stream" .= streamName
                , "line" .= line
                , "role" .= role
                , "command" .= label
                ]
            }
        }
    decorateLine streamName line =
      "[" <> label <> "][" <> streamName <> "] " <> line

    mergeEnv additions baseEnv = additions ++ filter ((`notElem` keys) . fst) baseEnv
      where
        keys = fmap fst additions
