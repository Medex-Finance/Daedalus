{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module App.API where

import App.Foundation
import App.Models
import App.Orchestrator
  ( cancelTask
  , enqueueTask
  , enqueueTaskStep
  , forceRetry
  , logEvent
  , orchestratorSnapshot
  , pauseTask
  , reassignTask
  , redirectWorker
  , resumeTask
  , resumeTaskAfterMessage
  , defaultStepForRole
  , hintStepFor
  , scheduleTaskSnooze
  , cancelTaskSnooze
  , lookupTaskSnooze
  , taskSummaryFromEntity
  , withOrigin
  )
import App.Preview (recordPreviewPing, startPreview, teardownPreview)
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
import Control.Applicative ((<|>))
import Control.Monad (forever, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Data.List (splitAt)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as BL
import Data.Time.Clock (getCurrentTime)
import Text.Read (readMaybe)
import Database.Persist
  ( Entity(..)
  , Key
  , SelectOpt(..)
  , entityKey
  , entityVal
  , get
  , insert
  , selectList
  , update
  , (=.)
  , (==.)
  , (<.)
  )
import Database.Persist.Sql (SqlPersistT, fromSqlKey, toSqlKey)
import Network.HTTP.Types.Status (status400)
import Yesod
import Yesod.Core (sendChunkText, sendFlush)

-- Task endpoints ------------------------------------------------------------

getTaskListR :: Handler Value
getTaskListR = do
  app@App { appPausedTasks, appSnoozedTasks } <- getYesod
  pausedSet <- liftIO $ readTVarIO appPausedTasks
  snoozedMap <- liftIO $ readTVarIO appSnoozedTasks
  tasks <- runDB $ selectList [] [Desc TaskUpdatedAt]
  returnJson (fmap (taskSummaryFromEntity pausedSet snoozedMap) tasks)

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
    , taskTestCommandOverride = Nothing
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
      returnJson (taskSummaryFromEntity Set.empty Map.empty (Entity taskId task))

getTaskR :: TaskId -> Handler Value
getTaskR taskId = do
  App {..} <- getYesod
  defaults <- liftIO $ readTVarIO appSettingsVar
  pausedSet <- liftIO $ readTVarIO appPausedTasks
  snoozedMap <- liftIO $ readTVarIO appSnoozedTasks
  mTask <- runDB $ get taskId
  case mTask of
    Nothing -> notFound
    Just task -> do
      runs <- runDB $ selectList [TaskRunTaskId ==. taskId] [Desc TaskRunOrdinal]
      (eventPage, nextCursor) <- runDB $ fetchEventPage taskId Nothing
      artifacts <- runDB $ selectList [ArtifactTaskId ==. taskId] [Desc ArtifactCreatedAt]
      let overrideCmd = taskTestCommandOverride task
          effectiveCmd = fromMaybe (settingsTestCommand defaults) overrideCmd
      let detail = TaskDetail
            { taskDetailSummary = taskSummaryFromEntity pausedSet snoozedMap (Entity taskId task)
            , taskDetailRuns = fmap taskRunToInfo runs
            , taskDetailEvents = fmap statusEventToDTO eventPage
            , taskDetailArtifacts = fmap artifactToDTO artifacts
            , taskDetailTestCommand = effectiveCmd
            , taskDetailTestCommandOverride = overrideCmd
            , taskDetailIsPaused = Set.member taskId pausedSet
            , taskDetailSnoozeUntil = Map.lookup taskId snoozedMap
            , taskDetailEventNextCursor = fmap fromSqlKey nextCursor
            }
      returnJson detail

eventPageSize :: Int
eventPageSize = 200

fetchEventPage
  :: MonadIO m
  => TaskId
  -> Maybe (Key StatusEvent)
  -> SqlPersistT m ([Entity StatusEvent], Maybe (Key StatusEvent))
fetchEventPage taskId beforeCursor = do
  let baseFilters = [StatusEventTaskId ==. taskId]
      cursorFilters = maybe [] (\cursor -> [StatusEventId <. cursor]) beforeCursor
      filters = baseFilters <> cursorFilters
  events <- selectList filters [Desc StatusEventId, LimitTo (eventPageSize + 1)]
  let (pageItems, rest) = splitAt eventPageSize events
      nextCursor = case (pageItems, rest) of
        ([], _) -> Nothing
        (_, []) -> Nothing
        (_, _) -> Just (entityKey (last pageItems))
  pure (pageItems, nextCursor)

readCursorParam :: Maybe Text -> Either Text (Maybe (Key StatusEvent))
readCursorParam Nothing = Right Nothing
readCursorParam (Just txt) =
  case readMaybe (T.unpack txt) of
    Nothing -> Left "Invalid cursor"
    Just ident -> Right (Just (toSqlKey ident))

getTaskHistoryR :: TaskId -> Handler Value
getTaskHistoryR taskId = do
  beforeParam <- lookupGetParam "beforeId"
  case readCursorParam beforeParam of
    Left err -> sendResponseStatus status400 (object ["error" .= err])
    Right cursor -> do
      (eventPage, nextCursor) <- runDB $ fetchEventPage taskId cursor
      let page = TaskHistoryPage
            { taskHistoryEvents = fmap statusEventToDTO eventPage
            , taskHistoryNextCursor = fmap fromSqlKey nextCursor
            }
      returnJson page

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

putTaskTestCommandR :: TaskId -> Handler Value
putTaskTestCommandR taskId = do
  app@App {..} <- getYesod
  TaskTestCommandUpdateRequest {..} <- requireCheckJsonBody
  settings <- liftIO $ readTVarIO appSettingsVar
  now <- liftIO getCurrentTime
  let normalized = fmap T.strip taskTestCommand
      effective = fromMaybe (settingsTestCommand settings) normalized
      payload = Just $ object
        [ "override" .= normalized
        , "effective" .= effective
        ]
      message = case normalized of
        Nothing -> "Test command override cleared. Using global default."
        Just cmd -> "Test command override updated to " <> cmd
  runDB $ update taskId
    [ TaskTestCommandOverride =. normalized
    , TaskUpdatedAt =. now
    ]
  liftIO $ logEvent app taskId StepImplementation message payload
  getTaskR taskId

postTaskCancelR :: TaskId -> Handler Value
postTaskCancelR taskId = do
  app <- getYesod
  liftIO $ cancelTask app taskId
  getTaskR taskId

postTaskQaSkipR :: TaskId -> Handler Value
postTaskQaSkipR taskId = do
  app@App { appConnPool } <- getYesod
  now <- liftIO getCurrentTime
  liftIO $ runSqlPool
    (update taskId
      [ TaskStatus =. TaskStatusReviewing
      , TaskUpdatedAt =. now
      ])
    appConnPool
  liftIO $ logEvent app taskId StepQaReview "QA step skipped by human" Nothing
  liftIO $ enqueueTaskStep app taskId StepCommit
  getTaskR taskId

postTaskPreviewStartR :: TaskId -> Handler Value
postTaskPreviewStartR taskId = do
  app@App { appConnPool } <- getYesod
  outcome <- liftIO $ startPreview app taskId
  case outcome of
    Left err -> sendResponseStatus status400 (object ["error" .= err])
    Right url -> do
      liftIO $ logEvent app taskId StepPreview "Preview launch requested manually" (Just $ object ["url" .= url])
      liftIO $ enqueueTaskStep app taskId StepFinalize
      getTaskR taskId

postTaskMessageR :: TaskId -> Handler Value
postTaskMessageR taskId = do
  app <- getYesod
  AgentMessageRequest {..} <- requireCheckJsonBody
  let trimmed = T.strip agentMessage
      requestedStep = agentMessageNextStep
      requestedRole = agentMessageRole
      autoResume = fromMaybe True agentMessageAutoResume
      logStep = hintStepFor (fromMaybe StepFixIteration (requestedStep <|> defaultStepForRole requestedRole))
      payload = Just $ object
        [ "message" .= trimmed
        , "role" .= requestedRole
        , "requestedStep" .= requestedStep
        , "autoResume" .= autoResume
        , "origin" .= ("human" :: Text)
        ]
  liftIO $ logEvent app taskId logStep "Human guidance received" payload
  resumed <- liftIO $ resumeTaskAfterMessage app taskId trimmed requestedRole requestedStep autoResume "human"
  returnJson (object ["ok" .= True, "resumed" .= resumed])

postTaskRetryR :: TaskId -> Handler Value
postTaskRetryR taskId = do
  app <- getYesod
  DTO.TaskRetryRequest { DTO.taskRetryInstructions } <- requireCheckJsonBody
  outcome <- liftIO $ forceRetry app taskId taskRetryInstructions
  case outcome of
    Left err -> sendResponseStatus status400 (object ["error" .= err])
    Right step -> returnJson (object ["requeuedStep" .= step])

postTaskPauseR :: TaskId -> Handler Value
postTaskPauseR taskId = do
  app <- getYesod
  runtime <- liftIO $ pauseTask app taskId
  let payload = case runtime of
        Nothing -> Nothing
        Just AgentRuntime { agentRole, agentStep } ->
          Just $ object
            [ "interruptedRole" .= agentRole
            , "interruptedStep" .= agentStep
            ]
  liftIO $ logEvent app taskId StepFixIteration "Task paused by human" (withOrigin "human" payload)
  returnJson (object ["paused" .= True])

postTaskResumeR :: TaskId -> Handler Value
postTaskResumeR taskId = do
  app <- getYesod
  liftIO $ resumeTask app taskId
  liftIO $ logEvent app taskId StepFixIteration "Task resume requested by human" Nothing
  returnJson (object ["paused" .= False])

postTaskReassignR :: TaskId -> Handler Value
postTaskReassignR taskId = do
  app <- getYesod
  outcome <- liftIO $ reassignTask app taskId
  case outcome of
    Left err -> sendResponseStatus status400 (object ["error" .= err])
    Right step -> returnJson (object ["requeuedStep" .= step])

postTaskRedirectR :: TaskId -> Handler Value
postTaskRedirectR taskId = do
  app <- getYesod
  DTO.TaskRedirectRequest { DTO.taskRedirectTargetTaskId } <- requireCheckJsonBody
  let targetKey = toSqlKey (fromIntegral taskRedirectTargetTaskId)
  outcome <- liftIO $ redirectWorker app taskId targetKey
  case outcome of
    Left err -> sendResponseStatus status400 (object ["error" .= err])
    Right response -> returnJson response

postTaskSnoozeR :: TaskId -> Handler Value
postTaskSnoozeR taskId = do
  app <- getYesod
  DTO.TaskSnoozeRequest { DTO.taskSnoozeMinutes } <- requireCheckJsonBody
  let minutes = max 1 taskSnoozeMinutes
  resumeAt <- liftIO $ scheduleTaskSnooze app taskId minutes
  returnJson DTO.TaskSnoozeStatus
    { DTO.taskSnoozeUntil = Just resumeAt
    }

deleteTaskSnoozeR :: TaskId -> Handler Value
deleteTaskSnoozeR taskId = do
  app <- getYesod
  mExisting <- liftIO $ lookupTaskSnooze app taskId
  liftIO $ cancelTaskSnooze app taskId
  when (isJust mExisting) $
    liftIO $ logEvent app taskId StepFixIteration "Snooze cancelled by human" (Just $ object ["origin" .= ("human" :: Text)])
  returnJson DTO.TaskSnoozeStatus
    { DTO.taskSnoozeUntil = Nothing
    }

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

statusEventToDTO :: Entity StatusEvent -> StatusEventDTO
statusEventToDTO (Entity key StatusEvent { statusEventStep = step, statusEventMessage = message, statusEventCreatedAt = created, statusEventPayload = payload }) =
  StatusEventDTO
    { statusEventId = Just (fromSqlKey key)
    , statusEventStep = step
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
