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
import App.ProcessDispatcher (launchServiceInDir)
import App.Types (AgentRole(..), AppSettingsDTO(..), ArtifactKind(..), PreviewStatus(..), WorkflowStep(..))
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
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (ProcessHandle, getPid, proc, readCreateProcessWithExitCode)

previewBasePort :: Int
previewBasePort = 43000

previewPortForTask :: TaskId -> Int
previewPortForTask taskId = previewBasePort + fromIntegral (fromSqlKey taskId `mod` 5000)

previewUrlForTask :: TaskId -> Text
previewUrlForTask taskId =
  T.pack $ "http://localhost:" <> show (previewPortForTask taskId)

startPreview :: App -> TaskId -> IO (Either Text Text)
startPreview app@App { appConnPool, appSettingsVar } taskId = do
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
          launchResult <- launchPreviewProcess app taskId worktree port cmd
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

launchPreviewProcess :: App -> TaskId -> WorktreeContext -> Int -> Text -> IO (Either Text (Int, FilePath))
launchPreviewProcess app taskId WorktreeContext { wtRoot } port commandText = do
  timestamp <- getCurrentTime
  createDirectoryIfMissing True previewDir
  TIO.appendFile logPath $ T.pack ("\n=== Preview launch " <> show timestamp <> " ===\n")
  let envExtras =
        [ ("PORT", show port)
        , ("PREVIEW_PORT", show port)
        ]
  result <- launchServiceInDir app taskId StepPreview AgentRoleImplementer "preview" wtRoot logPath envExtras commandText
  case result of
    Left err -> pure (Left err)
    Right handle -> do
      mPid <- getPid handle
      case mPid of
        Nothing -> pure $ Left "Unable to determine preview PID"
        Just pid -> pure $ Right (fromIntegral pid, logPath)
  where
    previewDir = wtRoot </> ".preview"
    logPath = previewDir </> "preview.log"

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
