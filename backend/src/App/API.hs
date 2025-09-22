{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module App.API where

import App.Foundation
import App.Models
import App.Orchestrator (cancelTask, enqueueTask, logEvent, orchestratorSnapshot)
import App.Preview (recordPreviewPing, teardownPreview)
import App.PromptStore
  ( getPromptTemplate
  , listPromptTemplates
  , resetPromptTemplate
  , updatePromptTemplate
  )
import App.Settings (updateSettings)
import App.StatusStream (StatusEnvelope(..), subscribeStatus)
import App.Types hiding (promptTemplateKey, taskStatus)
import qualified App.Types as DTO
import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , readTChan
  , readTVarIO
  , writeTVar
  )
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as BL
import Data.Time.Clock (getCurrentTime)
import Database.Persist
  ( Entity(..)
  , SelectOpt(..)
  , entityVal
  , get
  , insert
  , selectList
  , update
  , (=.)
  , (==.)
  )
import Database.Persist.Sql (fromSqlKey)
import Network.HTTP.Types.Status (status400)
import Yesod
import Yesod.Core (sendChunkText, sendFlush)

-- Task endpoints ------------------------------------------------------------

getTaskListR :: Handler Value
getTaskListR = do
  tasks <- runDB $ selectList [] [Desc TaskUpdatedAt]
  returnJson (fmap taskSummaryFromEntity tasks)

postTaskListR :: Handler Value
postTaskListR = do
  app@App {..} <- getYesod
  TaskCreateRequest {..} <- requireCheckJsonBody
  defaults <- liftIO $ readTVarIO appSettingsVar
  now <- liftIO getCurrentTime
  let repo = fromMaybe (settingsDefaultRepoRoot defaults) taskReqRepoRoot
      branch = fromMaybe (settingsDefaultBranch defaults) taskReqBranch
  taskId <- runDB $ insert Task
    { taskTitle = taskReqTitle
    , taskDescription = taskReqDescription
    , taskStatus = TaskStatusPending
    , taskRepoRoot = repo
    , taskBranch = branch
    , taskFeatureBranch = Nothing
    , taskPreviewUrl = Nothing
    , taskPreviewStatus = PreviewOffline
    , taskCreatedAt = now
    , taskUpdatedAt = now
    }
  mTask <- runDB $ get taskId
  case mTask of
    Nothing -> sendResponseStatus status400 (object ["error" .= ("Failed to create task" :: Text)])
    Just task -> do
      liftIO $ enqueueTask app taskId
      returnJson (taskSummaryFromEntity (Entity taskId task))

getTaskR :: TaskId -> Handler Value
getTaskR taskId = do
  mTask <- runDB $ get taskId
  case mTask of
    Nothing -> notFound
    Just task -> do
      runs <- runDB $ selectList [TaskRunTaskId ==. taskId] [Desc TaskRunOrdinal]
      events <- runDB $ selectList [StatusEventTaskId ==. taskId] [Desc StatusEventCreatedAt]
      artifacts <- runDB $ selectList [ArtifactTaskId ==. taskId] [Desc ArtifactCreatedAt]
      let detail = TaskDetail
            { taskDetailSummary = taskSummaryFromEntity (Entity taskId task)
            , taskDetailRuns = fmap taskRunToInfo runs
            , taskDetailEvents = fmap statusEventToDTO events
            , taskDetailArtifacts = fmap artifactToDTO artifacts
            }
      returnJson detail

patchTaskR :: TaskId -> Handler Value
patchTaskR taskId = do
  app@App {..} <- getYesod
  DTO.TaskUpdateStatusRequest { DTO.taskStatus = newStatus } <- requireCheckJsonBody
  now <- liftIO getCurrentTime
  runDB $ update taskId
    [ TaskStatus =. newStatus
    , TaskUpdatedAt =. now
    ]
  when (newStatus `elem` [TaskStatusCompleted, TaskStatusDiscarded]) $
    liftIO $ teardownPreview appConnPool taskId
  liftIO $ logEvent app taskId StepFinalize ("Status updated by human: " <> showText newStatus) Nothing
  getTaskR taskId

postTaskCancelR :: TaskId -> Handler Value
postTaskCancelR taskId = do
  app <- getYesod
  liftIO $ cancelTask app taskId
  getTaskR taskId

postTaskMessageR :: TaskId -> Handler Value
postTaskMessageR taskId = do
  app <- getYesod
  AgentMessageRequest {..} <- requireCheckJsonBody
  liftIO $ logEvent app taskId StepFixIteration ("Human message: " <> agentMessage) Nothing
  returnJson (object ["ok" .= True])

getTaskStatusStreamR :: TaskId -> Handler TypedContent
getTaskStatusStreamR taskId = do
  App {..} <- getYesod
  chan <- liftIO $ subscribeStatus appStatusHub
  respondSource "text/event-stream" $ forever $ do
    envelope <- liftIO $ atomically $ readTChan chan
    when (envelopeTaskId envelope == taskId) $ do
      let payloadText = decodeUtf8 . BL.toStrict $ Aeson.encode (envelopeEvent envelope)
      sendChunkText "data: "
      sendChunkText payloadText
      sendChunkText "\n\n"
      sendFlush

-- Settings -----------------------------------------------------------------

getSettingsR :: Handler Value
getSettingsR = do
  App {..} <- getYesod
  settings <- liftIO $ readTVarIO appSettingsVar
  returnJson settings

putSettingsR :: Handler Value
putSettingsR = do
  app@App {..} <- getYesod
  req <- requireCheckJsonBody
  updated <- liftIO $ updateSettings appConnPool req
  liftIO $ atomically $ writeTVar appSettingsVar updated
  returnJson updated

-- Prompts ------------------------------------------------------------------

getPromptListR :: Handler Value
getPromptListR = do
  App {..} <- getYesod
  templates <- liftIO $ listPromptTemplates appConnPool
  returnJson templates

getPromptR :: Text -> Handler Value
getPromptR key = do
  App {..} <- getYesod
  result <- liftIO $ getPromptTemplate appConnPool key
  maybe notFound returnJson result

putPromptR :: Text -> Handler Value
putPromptR key = do
  app@App {..} <- getYesod
  req <- requireCheckJsonBody
  outcome <- liftIO $ updatePromptTemplate appConnPool key req (Just "admin")
  case outcome of
    Left err -> sendResponseStatus status400 (object ["error" .= err])
    Right dto -> do
      liftIO $ atomically $ modifyTVar' appPromptCache (Map.insert (DTO.promptTemplateKey dto) dto)
      returnJson dto

postPromptResetR :: Text -> Handler Value
postPromptResetR key = do
  app@App {..} <- getYesod
  outcome <- liftIO $ resetPromptTemplate appConnPool key (Just "reset")
  case outcome of
    Left err -> sendResponseStatus status400 (object ["error" .= err])
    Right dto -> do
      liftIO $ atomically $ modifyTVar' appPromptCache (Map.insert (DTO.promptTemplateKey dto) dto)
      returnJson dto

getPromptRevisionsR :: Text -> Handler Value
getPromptRevisionsR key = do
  revisions <- runDB $ selectList [PromptRevisionTemplateKey ==. key] [Desc PromptRevisionVersion]
  returnJson (fmap revisionToDTO revisions)

-- Snapshot / Preview -------------------------------------------------------

getSnapshotR :: Handler Value
getSnapshotR = do
  app <- getYesod
  snapshot <- liftIO $ orchestratorSnapshot app
  returnJson snapshot

postPreviewPingR :: TaskId -> Handler Value
postPreviewPingR taskId = do
  App {..} <- getYesod
  mTask <- runDB $ get taskId
  case mTask of
    Nothing -> notFound
    Just task -> case taskPreviewUrl task of
      Nothing -> sendResponseStatus status400 (object ["error" .= ("Preview not registered" :: Text)])
      Just url -> do
        status <- liftIO $ recordPreviewPing appConnPool appManager taskId url
        returnJson (object ["status" .= status])

-- Conversions --------------------------------------------------------------

taskSummaryFromEntity :: Entity Task -> TaskSummary
taskSummaryFromEntity (Entity key task) =
  TaskSummary
    { taskSummaryId = fromIntegral (fromSqlKey key)
    , taskSummaryTitle = taskTitle task
    , taskSummaryStatus = taskStatus task
    , taskSummaryRepoRoot = taskRepoRoot task
    , taskSummaryBranch = taskBranch task
    , taskSummaryFeatureBranch = taskFeatureBranch task
    , taskSummaryUpdatedAt = taskUpdatedAt task
    , taskSummaryPreviewUrl = taskPreviewUrl task
    , taskSummaryPreviewStatus = taskPreviewStatus task
    }

statusEventToDTO :: Entity StatusEvent -> StatusEventDTO
statusEventToDTO (Entity _ StatusEvent { statusEventStep = step, statusEventMessage = message, statusEventCreatedAt = created, statusEventPayload = payload }) =
  StatusEventDTO
    { statusEventStep = step
    , statusEventMessage = message
    , statusEventCreatedAt = created
    , statusEventPayload = payload
    }

artifactToDTO :: Entity Artifact -> ArtifactDTO
artifactToDTO (Entity _ Artifact { artifactKind = kind, artifactLabel = label, artifactContent = content, artifactPath = path, artifactCreatedAt = created }) =
  ArtifactDTO
    { artifactKind = kind
    , artifactLabel = label
    , artifactBody = content
    , artifactPath = path
    , artifactCreatedAt = created
    }

revisionToDTO :: Entity PromptRevision -> PromptRevisionDTO
revisionToDTO (Entity _ PromptRevision { promptRevisionTemplateKey = key, promptRevisionVersion = version, promptRevisionEditor = editor, promptRevisionContent = content, promptRevisionCreatedAt = created }) =
  PromptRevisionDTO
    { promptRevisionKey = key
    , promptRevisionVersion = version
    , promptRevisionEditor = editor
    , promptRevisionContent = content
    , promptRevisionCreatedAt = created
    }

taskRunToInfo :: Entity TaskRun -> TaskRunInfo
taskRunToInfo (Entity _ TaskRun { taskRunOrdinal = ord, taskRunCurrentStep = step, taskRunPmSummary = summary, taskRunCreatedAt = created, taskRunUpdatedAt = updated }) =
  TaskRunInfo
    { taskRunOrdinal = ord
    , taskRunCurrentStep = step
    , taskRunPmSummary = summary
    , taskRunCreatedAt = created
    , taskRunUpdatedAt = updated
    }

showText :: Show a => a -> Text
showText = T.pack . show
