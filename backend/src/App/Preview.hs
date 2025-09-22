{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module App.Preview
  ( startPreview
  , teardownPreview
  , recordPreviewPing
  , previewUrlForTask
  ) where

import App.Foundation (App(..))
import App.Models
import App.Types (AppSettingsDTO(..), ArtifactKind(..), PreviewStatus(..))
import App.Worktree (WorktreeContext(..), locateWorktree)
import Control.Concurrent.STM (readTVarIO)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (object, (.=))
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.Persist.Sql (SqlPersistT, runSqlPool)
import Network.HTTP.Client (Manager, httpLbs, parseRequest, responseStatus)
import Network.HTTP.Types.Status (status200)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)
import Text.Read (readMaybe)

previewBasePort :: Int
previewBasePort = 43000

previewPortForTask :: TaskId -> Int
previewPortForTask taskId = previewBasePort + fromIntegral (fromSqlKey taskId `mod` 5000)

previewUrlForTask :: TaskId -> Text
previewUrlForTask taskId =
  T.pack $ "http://localhost:" <> show (previewPortForTask taskId)

startPreview :: App -> TaskId -> IO (Either Text Text)
startPreview App { appConnPool, appSettingsVar } taskId = do
  mTask <- runSqlPool (get taskId) appConnPool
  case mTask of
    Nothing -> pure (Left "Task not found")
    Just task -> do
      AppSettingsDTO { settingsPreviewCommand = configuredCommand } <- readTVarIO appSettingsVar
      let mCommand = T.strip <$> configuredCommand
      case mCommand of
        Nothing -> failWith "Preview command not configured"
        Just cmd | T.null cmd -> failWith "Preview command not configured"
        Just cmd -> do
          teardownPreview appConnPool taskId
          let port = previewPortForTask taskId
              url = previewUrlForTask taskId
          worktree <- locateWorktree (T.unpack (taskRepoRoot task)) taskId
          launchResult <- launchPreviewProcess worktree port cmd
          case launchResult of
            Left err -> failWith err
            Right (pid, logPath) -> do
              now <- getCurrentTime
              runSqlPool
                (do
                  update taskId
                    [ TaskPreviewUrl =. Just url
                    , TaskPreviewStatus =. PreviewLaunching
                    , TaskUpdatedAt =. now
                    ]
                  deleteWhere [PreviewProcessTaskId ==. taskId]
                  _ <- insert PreviewProcess
                    { previewProcessTaskId = taskId
                    , previewProcessPort = port
                    , previewProcessPid = Just pid
                    , previewProcessStatus = PreviewLaunching
                    , previewProcessLastHealthCheck = Just now
                    , previewProcessCreatedAt = now
                    }
                  storeLaunchArtifact "preview-launch" taskId cmd port pid (T.pack logPath) now
                ) appConnPool
              pure (Right url)
  where
    failWith :: Text -> IO (Either Text Text)
    failWith msg = do
      now <- getCurrentTime
      runSqlPool
        (do
          update taskId
            [ TaskPreviewStatus =. PreviewFailed
            , TaskPreviewUrl =. Nothing
            , TaskUpdatedAt =. now
            ]
          deleteWhere [PreviewProcessTaskId ==. taskId]
          storeLaunchArtifact "preview-launch-failed" taskId msg 0 0 T.empty now
        ) appConnPool
      pure (Left msg)

storeLaunchArtifact :: Text -> TaskId -> Text -> Int -> Int -> Text -> UTCTime -> SqlPersistT IO ()
storeLaunchArtifact label tid info port pid logPath now =
  insert_ Artifact
    { artifactTaskId = tid
    , artifactKind = ArtifactPreviewLog
    , artifactLabel = label
    , artifactContent = Just $ object
        [ "info" .= info
        , "port" .= port
        , "pid" .= pid
        , "logPath" .= logPath
        ]
    , artifactPath = if T.null logPath then Nothing else Just logPath
    , artifactCreatedAt = now
    }

launchPreviewProcess :: WorktreeContext -> Int -> Text -> IO (Either Text (Int, FilePath))
launchPreviewProcess WorktreeContext { wtRoot } port commandText = do
  env <- getEnvironment
  timestamp <- getCurrentTime
  createDirectoryIfMissing True previewDir
  TIO.appendFile logPath $ T.pack ("\n=== Preview launch " <> show timestamp <> " ===\n")
  let processEnv = mergeEnv
        [ ("PORT", show port)
        , ("PREVIEW_PORT", show port)
        ]
        env
      script = T.unlines
        [ "set -euo pipefail"
        , "mkdir -p .preview"
        , "(" <> commandText <> ") >> .preview/preview.log 2>&1 &"
        , "echo $!"
        ]
  (code, out, err) <- readCreateProcessWithExitCode (proc "bash" ["-lc", T.unpack script])
    { cwd = Just wtRoot
    , env = Just processEnv
    } ""
  case code of
    ExitSuccess ->
      case readMaybe (T.unpack . T.strip $ T.pack out) of
        Nothing -> pure $ Left "Unable to parse preview PID"
        Just pid -> pure $ Right (pid, logPath)
    _ -> pure . Left $ T.strip (T.pack err)
  where
    previewDir = wtRoot </> ".preview"
    logPath = previewDir </> "preview.log"

mergeEnv :: [(String, String)] -> [(String, String)] -> [(String, String)]
mergeEnv additions base = additions ++ filter ((`notElem` keys) . fst) base
  where
    keys = fmap fst additions

teardownPreview :: ConnectionPool -> TaskId -> IO ()
teardownPreview pool taskId = do
  processes <- runSqlPool (selectList [PreviewProcessTaskId ==. taskId] []) pool
  for_ processes $ \(Entity _ previewProc) ->
    for_ (previewProcessPid previewProc) $ \pid ->
      void $ try @SomeException $ readCreateProcessWithExitCode (proc "bash" ["-lc", "kill -TERM " <> show pid]) ""
  now <- getCurrentTime
  runSqlPool
    (do
      update taskId
        [ TaskPreviewStatus =. PreviewOffline
        , TaskPreviewUrl =. Nothing
        , TaskUpdatedAt =. now
        ]
      deleteWhere [PreviewProcessTaskId ==. taskId]
    ) pool

recordPreviewPing :: ConnectionPool -> Manager -> TaskId -> Text -> IO PreviewStatus
recordPreviewPing pool manager taskId url = do
  status <- ping manager url
  now <- getCurrentTime
  runSqlPool
    (do
      update taskId
        [ TaskPreviewStatus =. status
        , TaskUpdatedAt =. now
        ]
      updateWhere [PreviewProcessTaskId ==. taskId]
        [ PreviewProcessStatus =. status
        , PreviewProcessLastHealthCheck =. Just now
        ]
      _ <- insert Artifact
        { artifactTaskId = taskId
        , artifactKind = ArtifactPreviewPing
        , artifactLabel = "preview-ping"
        , artifactContent = Just $ object
            [ "url" .= url
            , "status" .= show status
            ]
        , artifactPath = Nothing
        , artifactCreatedAt = now
        }
      pure status
    ) pool

ping :: Manager -> Text -> IO PreviewStatus
ping manager url = do
  result <- try @SomeException $ do
    req <- parseRequest (T.unpack url)
    resp <- httpLbs req manager
    pure $ if responseStatus resp == status200 then PreviewOnline else PreviewFailed
  pure $ case result of
    Left _ -> PreviewFailed
    Right status -> status
