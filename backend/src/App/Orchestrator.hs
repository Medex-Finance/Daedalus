{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module App.Orchestrator
  ( startOrchestrator
  , enqueueTask
  , enqueueTaskStep
  , orchestratorSnapshot
  , logEvent
  , cancelTask
  , resumeTaskAfterMessage
  , defaultStepForRole
  , hintStepFor
  , pauseTask
  , resumeTask
  , forceRetry
  , reassignTask
  , redirectWorker
  , scheduleTaskSnooze
  , cancelTaskSnooze
  , lookupTaskSnooze
  , taskSummaryFromEntity
  , withOrigin
  , runSpecVerification
  ) where

import App.AgentGateway
import App.Evidence
  ( EvidenceCapture(..)
  , EvidenceOptions(..)
  , collectDemoEvidence
  , defaultEvidenceOptions
  )
import App.RepoProfiles
  ( RepoProfile(..)
  , RepoProfiles(..)
  , lookupRepoProfile
  , runRepoProfileSetup
  )
import App.Gemini (GeminiVerdict(..), runGeminiReview)
import App.Foundation (AgentRuntime(..), App(..), WorkerMetrics(..), WorkerRuntimeState(..))
import App.Logging (logError, logInfo, logWarn)
import App.Models
import qualified App.Models as Models
import App.Preview
import App.Queue
  ( QueueMessage(..)
  , dequeueForWorker
  , enqueueForWorker
  , enqueueGlobal
  , queueSize
  )
import App.StatusStream
import App.Types hiding
  ( artifactCreatedAt
  , artifactKind
  , artifactLabel
  , artifactPath
  , promptRevisionContent
  , promptRevisionCreatedAt
  , promptRevisionEditor
  , promptRevisionVersion
  , promptTemplateContent
  , promptTemplateDescription
  , promptTemplateIsCustom
  , promptTemplateKey
  , promptTemplateUpdatedAt
  , promptTemplateVersion
  , taskRunOrdinal
  , taskStatus
  )
import App.Worktree
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (atomically, modifyTVar', readTVar, readTVarIO, writeTVar, retry)
import Control.Applicative ((<|>))
import Control.Monad (forM_, forever, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
  ( FromJSON(..)
  , Value(..)
  , eitherDecode
  , object
  , withObject
  , (.:)
  , (.:?)
  , (.=)
  , toJSON
  )
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, isNothing)
import Text.Read (readMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as BL
import Control.Exception (SomeException, displayException, throwIO, try)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode, terminateProcess)

startOrchestrator :: App -> IO ()
startOrchestrator app@App { appWorkerStates, appWorkerMetrics, appWorkerCount } = do
  let workerIds = [1 .. appWorkerCount]
  now <- getCurrentTime
  atomically $ do
    let initialStates = Map.fromList (map (\workerId -> (workerId, WorkerRuntimeIdle now)) workerIds)
        initialMetrics = Map.fromList (map (\workerId -> (workerId, defaultWorkerMetrics)) workerIds)
    writeTVar appWorkerStates initialStates
    writeTVar appWorkerMetrics initialMetrics
  pauseActiveTasksOnStartup app
  forM_ workerIds $ \workerId -> void (async (workerLoop app workerId))
  _ <- async (inactivityMonitor app)
  _ <- async (snoozeMonitor app)
  _ <- async (repoSyncMonitor app)
  _ <- async (previewMonitor app)
  logInfo $ "Orchestrator workers started (count=" <> showText appWorkerCount <> ")"
  pure ()

pauseActiveTasksOnStartup :: App -> IO ()
pauseActiveTasksOnStartup app@App { appConnPool, appPausedTasks } = do
  active <-
    runSqlPool
      (selectList
        [ TaskStatus !=. TaskStatusCompleted
        , TaskStatus !=. TaskStatusCancelled
        , TaskStatus !=. TaskStatusDiscarded
        ]
        [])
      appConnPool
  let activeIds = Set.fromList (map entityKey active)
  atomically $ writeTVar appPausedTasks activeIds
  for_ active $ \(Entity taskId _) ->
    logEvent app taskId StepFixIteration "Task paused on startup; resume manually"
      (withOrigin "system" Nothing)

data AssignmentResult
  = AssignmentClaimed
  | AssignmentAlreadyOwned
  | AssignmentOwnedBy Int

workerLoop :: App -> Int -> IO ()
workerLoop app@App { appQueue } workerId = forever $ do
  hasTask <- workerHasAssignment app workerId
  unless hasTask $ markWorkerIdle app workerId
  message <- dequeueForWorker appQueue workerId
  waitForResume app workerId message
  case message of
    msg@(QueueKickoff taskId) ->
      handleMessage msg taskId StepIntake (processKickoff app taskId)
    msg@(QueueAdvance taskId step) ->
      handleMessage msg taskId step (processStep app taskId step)
  where
    handleMessage originalMsg taskId step action = do
      assignmentResult <- assignTaskToWorker app taskId workerId
      case assignmentResult of
        AssignmentOwnedBy otherWorker -> do
          enqueueForWorker appQueue otherWorker originalMsg
        _ -> do
          markWorkerRunning app workerId taskId step
          startedAt <- recordWorkerStepStart app workerId taskId step
          result <- try @SomeException action
          finishedAt <- getCurrentTime
          let (success, errText) =
                case result of
                  Left err -> (False, Just (T.pack (displayException err)))
                  Right _ -> (True, Nothing)
          recordWorkerStepFinish app workerId taskId step success errText startedAt finishedAt
          case result of
            Left err -> throwIO err
            Right _ -> releaseIfFinished app workerId taskId

repoSyncMonitor :: App -> IO ()
repoSyncMonitor app = do
  intervalMicros <- determineInterval
  forever $ do
    syncResult <- try @SomeException (syncActiveBranches app)
    case syncResult of
      Left err ->
        logError $ "Repo sync tick failed: " <> T.pack (displayException err)
      Right () ->
        pure ()
    threadDelay intervalMicros
  where
    determineInterval = do
      env <- lookupEnv "REPO_SYNC_INTERVAL_SECONDS"
      let parsed = env >>= readMaybe
          seconds = clampInterval (fromMaybe 60 parsed)
      pure (seconds * 1000000)
    clampInterval s = max 10 (min 900 s)

previewMonitor :: App -> IO ()
previewMonitor app@App { appConnPool, appManager } =
  forever $ do
    processes <- runSqlPool (selectList [PreviewProcessStatus !=. PreviewFailed] []) appConnPool
    for_ processes $ \(Entity _ proc) -> do
      let taskId = previewProcessTaskId proc
          mPid = previewProcessPid proc
      alive <- maybe (pure True) isPreviewProcessAlive mPid
      unless alive $ do
        markPreviewFailed appConnPool taskId "Preview process exited unexpectedly" mPid
        logEvent app taskId StepPreview "Preview process exited unexpectedly"
          (Just $ object ["pid" .= mPid])
      when alive $ do
        mTask <- runSqlPool (get taskId) appConnPool
        for_ mTask $ \task -> do
          let url = fromMaybe (previewUrlForTask taskId) (taskPreviewUrl task)
          status <- pingPreview appManager url
          case status of
            PreviewOnline ->
              when (previewProcessStatus proc /= PreviewOnline || taskPreviewStatus task /= PreviewOnline) $ do
                markPreviewOnline appConnPool taskId
                logEvent app taskId StepPreview "Preview marked online"
                  (Just $ object ["url" .= url])
            PreviewFailed -> do
              markPreviewFailed appConnPool taskId "Preview health check failed" mPid
              logEvent app taskId StepPreview "Preview health check failed"
                (Just $ object ["url" .= url])
            _ -> pure ()
    threadDelay (20 * 1000000)

syncActiveBranches :: App -> IO ()
syncActiveBranches app@App { appConnPool, appSettingsVar, appAgentRegistry, appPausedTasks } = do
  settings <- readTVarIO appSettingsVar
  activeTasks <-
    runSqlPool
      (selectList
        [ TaskStatus !=. TaskStatusCompleted
        , TaskStatus !=. TaskStatusCancelled
        , TaskStatus !=. TaskStatusDiscarded
        ]
        [])
      appConnPool
  activeAgents <- readTVarIO appAgentRegistry
  paused <- readTVarIO appPausedTasks
  let defaultRepo = T.unpack (settingsDefaultRepoRoot settings)
      grouped =
        foldr
          (\entity@(Entity _ task) acc ->
            let repoRoot = T.unpack (taskRepoRoot task)
             in Map.insertWith (<>) repoRoot [entity] acc
          )
          Map.empty
          activeTasks
      withDefault =
        if Map.member defaultRepo grouped || null defaultRepo
          then grouped
          else Map.insert defaultRepo [] grouped
  for_ (Map.toList withDefault) $ \(repoRoot, tasks) ->
    syncRepoRoot app settings repoRoot tasks activeAgents paused

syncRepoRoot
  :: App
  -> AppSettingsDTO
  -> FilePath
  -> [Entity Task]
  -> Map.Map TaskId AgentRuntime
  -> Set.Set TaskId
  -> IO ()
syncRepoRoot app settings repoRoot taskEntities activeAgents pausedSet = do
  exists <- doesDirectoryExist repoRoot
  unless exists $
    logWarn $ "Repo sync skipped missing repo root " <> T.pack repoRoot
  when exists $ do
    _ <- runRawGit repoRoot ["fetch", "--prune", "origin"]
    for_ taskEntities $ \entity ->
      syncTaskBranch app settings repoRoot entity activeAgents pausedSet

syncTaskBranch
  :: App
  -> AppSettingsDTO
  -> FilePath
  -> Entity Task
  -> Map.Map TaskId AgentRuntime
  -> Set.Set TaskId
  -> IO ()
syncTaskBranch app@App { appConnPool } settings repoRoot (Entity taskId task) activeAgents pausedSet =
  case (taskFeatureBranch task, T.strip (taskBranch task)) of
    (Just featureBranch, baseBranch)
      | T.null baseBranch -> pure ()
      | T.null (T.strip featureBranch) -> pure ()
      | Map.member taskId activeAgents -> pure ()
      | Set.member taskId pausedSet -> pure ()
      | otherwise -> do
          prepareResult <- try @SomeException (prepareWorktree app repoRoot baseBranch taskId False)
          case prepareResult of
            Left err ->
              logWarn $
                "Repo sync failed to prepare worktree for task "
                  <> taskKeyText taskId
                  <> ": "
                  <> T.pack (displayException err)
            Right worktree -> do
              clean <- isWorktreeClean worktree
              unless clean $
                logInfo $
                  "Repo sync skipped dirty worktree for task "
                    <> taskKeyText taskId
              when clean $ do
                behind <- branchNeedsRebase worktree baseBranch
                when behind $ do
                  logEvent app taskId StepImplementation "Auto-sync merging latest base branch" (withOrigin "system" (Just $ object ["base" .= baseBranch]))
                  mergeOutcome <- mergeBaseIntoWorktree worktree baseBranch
                  case mergeOutcome of
                    Left errMsg -> do
                      logEvent app taskId StepImplementation "Auto-sync merge failed" (withOrigin "system" (Just $ object ["error" .= errMsg]))
                      scheduleFixIteration app taskId StepImplementation "Auto-sync merge failed; manual intervention required" (Just $ object ["error" .= errMsg])
                    Right () -> do
                      let defaultCommand = settingsTestCommand settings
                          testCommand = fromMaybe defaultCommand (taskTestCommandOverride task)
                      logEvent app taskId StepImplementation "Auto-sync running regression tests" (withOrigin "system" (Just $ object ["command" .= testCommand]))
                      tests <- runTestsInWorktree app taskId StepImplementation AgentRoleImplementer worktree testCommand
                      storeTestArtifact appConnPool taskId testCommand tests
                      case wrExitCode tests of
                        ExitSuccess -> do
                          pushResult <- pushFeatureBranch worktree featureBranch
                          case pushResult of
                            Left errMsg ->
                              logWarn $
                                "Auto-sync push failed for task "
                                  <> taskKeyText taskId
                                  <> ": "
                                  <> errMsg
                            Right () ->
                              logEvent app taskId StepImplementation "Auto-sync merged base branch and pushed feature branch" (withOrigin "system" (Just $ object ["branch" .= featureBranch, "base" .= baseBranch]))
                        failureCode -> do
                          let payload =
                                Just $
                                  object
                                    [ "exitCode" .= show failureCode
                                    , "command" .= testCommand
                                    , "branch" .= featureBranch
                                    , "base" .= baseBranch
                                    ]
                          logEvent app taskId StepImplementation "Auto-sync tests failed after merging base branch" (withOrigin "system" payload)
                          scheduleFixIteration app taskId StepImplementation "Auto-sync detected failing tests after updating from base branch" payload
    _ -> pure ()

isWorktreeClean :: WorktreeContext -> IO Bool
isWorktreeClean WorktreeContext { wtRoot } = do
  (code, stdoutText, _) <- runRawGit wtRoot ["status", "--porcelain"]
  let cleaned = T.strip (T.pack stdoutText)
  pure (code == ExitSuccess && T.null cleaned)

branchNeedsRebase :: WorktreeContext -> Text -> IO Bool
branchNeedsRebase WorktreeContext { wtRoot } baseBranch = do
  let ref = "origin/" <> T.unpack baseBranch
  (code, stdoutText, _) <- runRawGit wtRoot ["rev-list", "--count", "HEAD.." <> ref]
  case code of
    ExitSuccess ->
      case (readMaybe (T.unpack (T.strip (T.pack stdoutText))) :: Maybe Int) of
        Just count -> pure (count > 0)
        Nothing -> pure False
    _ -> pure False

mergeBaseIntoWorktree :: WorktreeContext -> Text -> IO (Either Text ())
mergeBaseIntoWorktree WorktreeContext { wtRoot } baseBranch = do
  let ref = "origin/" <> T.unpack baseBranch
  result <- runRawGit wtRoot ["merge", "--no-edit", ref]
  case result of
    (ExitSuccess, _, _) -> pure (Right ())
    (_, _, stderrText) -> do
      _ <- runRawGit wtRoot ["merge", "--abort"]
      pure . Left $ T.strip (T.pack stderrText)

pushFeatureBranch :: WorktreeContext -> Text -> IO (Either Text ())
pushFeatureBranch WorktreeContext { wtRoot } featureBranch = do
  let branchName = T.unpack featureBranch
  result <- runRawGit wtRoot ["push", "origin", branchName]
  case result of
    (ExitSuccess, _, _) -> pure (Right ())
    (_, _, stderrText) -> pure . Left $ T.strip (T.pack stderrText)

runRawGit :: FilePath -> [String] -> IO (ExitCode, String, String)
runRawGit dir args =
  readCreateProcessWithExitCode (proc "git" ("-C" : dir : args)) ""

messageTaskId :: QueueMessage -> TaskId
messageTaskId (QueueKickoff taskId) = taskId
messageTaskId (QueueAdvance taskId _) = taskId

waitForResume :: App -> Int -> QueueMessage -> IO ()
waitForResume app@App { appPausedTasks } workerId msg = do
  let taskId = messageTaskId msg
  paused <- isTaskPaused app taskId
  when paused $ do
    markWorkerIdle app workerId
    atomically $ do
      pausedTasks <- readTVar appPausedTasks
      when (Set.member taskId pausedTasks) retry
    waitForResume app workerId msg

markWorkerIdle :: App -> Int -> IO ()
markWorkerIdle App { appWorkerStates, appWorkerMetrics } workerId = do
  now <- getCurrentTime
  atomically $ do
    modifyTVar' appWorkerStates (Map.insert workerId (WorkerRuntimeIdle now))
    modifyTVar' appWorkerMetrics (Map.alter (Just . clearCurrent) workerId)
  where
    clearCurrent Nothing = defaultWorkerMetrics
    clearCurrent (Just metrics) = metrics
      { workerMetricsCurrentTask = Nothing
      , workerMetricsCurrentStep = Nothing
      , workerMetricsStartedAt = Nothing
      }

markWorkerRunning :: App -> Int -> TaskId -> WorkflowStep -> IO ()
markWorkerRunning App { appWorkerStates } workerId taskId step = do
  now <- getCurrentTime
  atomically $ modifyTVar' appWorkerStates (Map.insert workerId (WorkerRuntimeBusy taskId step now))

recordWorkerStepStart :: App -> Int -> TaskId -> WorkflowStep -> IO UTCTime
recordWorkerStepStart App { appWorkerMetrics } workerId taskId step = do
  now <- getCurrentTime
  let updateMetrics Nothing = defaultWorkerMetrics
        { workerMetricsCurrentTask = Just taskId
        , workerMetricsCurrentStep = Just step
        , workerMetricsStartedAt = Just now
        }
      updateMetrics (Just metrics) = metrics
        { workerMetricsCurrentTask = Just taskId
        , workerMetricsCurrentStep = Just step
        , workerMetricsStartedAt = Just now
        }
  atomically $ modifyTVar' appWorkerMetrics (Map.alter (Just . updateMetrics) workerId)
  pure now

recordWorkerStepFinish
  :: App
  -> Int
  -> TaskId
  -> WorkflowStep
  -> Bool
  -> Maybe Text
  -> UTCTime
  -> UTCTime
  -> IO ()
recordWorkerStepFinish App { appWorkerMetrics } workerId taskId step success err startedAt finishedAt = do
  let duration = max 0 (diffUTCTime finishedAt startedAt)
      updateMetrics Nothing = defaultWorkerMetrics
        { workerMetricsLastTask = Just taskId
        , workerMetricsLastStep = Just step
        , workerMetricsLastDuration = Just duration
        , workerMetricsLastSuccess = Just success
        , workerMetricsLastError = err
        , workerMetricsAssignments = 1
        , workerMetricsBusySeconds = duration
        }
      updateMetrics (Just metrics) = metrics
        { workerMetricsCurrentTask = Nothing
        , workerMetricsCurrentStep = Nothing
        , workerMetricsStartedAt = Nothing
        , workerMetricsLastTask = Just taskId
        , workerMetricsLastStep = Just step
        , workerMetricsLastDuration = Just duration
        , workerMetricsLastSuccess = Just success
        , workerMetricsLastError = err
        , workerMetricsAssignments = workerMetricsAssignments metrics + 1
        , workerMetricsBusySeconds = workerMetricsBusySeconds metrics + duration
        }
  atomically $ modifyTVar' appWorkerMetrics (Map.alter (Just . updateMetrics) workerId)

defaultWorkerMetrics :: WorkerMetrics
defaultWorkerMetrics = WorkerMetrics
  { workerMetricsCurrentTask = Nothing
  , workerMetricsCurrentStep = Nothing
  , workerMetricsStartedAt = Nothing
  , workerMetricsLastTask = Nothing
  , workerMetricsLastStep = Nothing
  , workerMetricsLastDuration = Nothing
  , workerMetricsLastSuccess = Nothing
  , workerMetricsLastError = Nothing
  , workerMetricsAssignments = 0
  , workerMetricsBusySeconds = 0
  }

workerHasAssignment :: App -> Int -> IO Bool
workerHasAssignment App { appWorkerAssignments } workerId = do
  assignments <- readTVarIO appWorkerAssignments
  pure (any (== workerId) (Map.elems assignments))

isTaskPaused :: App -> TaskId -> IO Bool
isTaskPaused App { appPausedTasks } taskId =
  Set.member taskId <$> readTVarIO appPausedTasks

pauseTask :: App -> TaskId -> IO (Maybe AgentRuntime)
pauseTask app@App { appPausedTasks } taskId = do
  mRuntime <- popAgentRuntime app taskId
  for_ mRuntime $ \runtime -> do
    terminateRuntime runtime
    enqueueStep app taskId (agentStep runtime)
  atomically $ modifyTVar' appPausedTasks (Set.insert taskId)
  pure mRuntime

resumeTask :: App -> TaskId -> IO ()
resumeTask App { appPausedTasks, appSnoozedTasks } taskId =
  atomically $ do
    modifyTVar' appPausedTasks (Set.delete taskId)
    modifyTVar' appSnoozedTasks (Map.delete taskId)

scheduleTaskSnooze :: App -> TaskId -> Int -> IO UTCTime
scheduleTaskSnooze app@App { appSnoozedTasks } taskId minutes = do
  let clamped = max 1 minutes
  now <- getCurrentTime
  let resumeAt = addUTCTime (fromIntegral (clamped * 60)) now
  cancelTaskSnooze app taskId
  _ <- pauseTask app taskId
  atomically $ modifyTVar' appSnoozedTasks (Map.insert taskId resumeAt)
  logEvent app taskId StepFixIteration "Task snoozed" (withOrigin "human" (Just $ object ["resumeAt" .= resumeAt, "minutes" .= clamped]))
  pure resumeAt

cancelTaskSnooze :: App -> TaskId -> IO ()
cancelTaskSnooze App { appSnoozedTasks } taskId =
  atomically $ modifyTVar' appSnoozedTasks (Map.delete taskId)

lookupTaskSnooze :: App -> TaskId -> IO (Maybe UTCTime)
lookupTaskSnooze App { appSnoozedTasks } taskId =
  Map.lookup taskId <$> readTVarIO appSnoozedTasks

assignTaskToWorker :: App -> TaskId -> Int -> IO AssignmentResult
assignTaskToWorker App { appWorkerAssignments } taskId workerId =
  atomically $ do
    assignments <- readTVar appWorkerAssignments
    case Map.lookup taskId assignments of
      Nothing -> do
        writeTVar appWorkerAssignments (Map.insert taskId workerId assignments)
        pure AssignmentClaimed
      Just existing
        | existing == workerId -> pure AssignmentAlreadyOwned
        | otherwise -> pure (AssignmentOwnedBy existing)

releaseTaskAssignment :: App -> TaskId -> IO ()
releaseTaskAssignment App { appWorkerAssignments } taskId =
  atomically $ modifyTVar' appWorkerAssignments (Map.delete taskId)

releaseIfFinished :: App -> Int -> TaskId -> IO ()
releaseIfFinished app@App { appConnPool } workerId taskId = do
  mTask <- runSqlPool (get taskId) appConnPool
  case mTask of
    Nothing -> releaseTaskAssignment app taskId >> markWorkerIdle app workerId
    Just task ->
      when (taskStatusHalting (taskStatus task) || taskStatus task == TaskStatusBlocked) $ do
        releaseTaskAssignment app taskId
        markWorkerIdle app workerId

workerStateToDTO :: App -> Int -> WorkerRuntimeState -> IO WorkerStatusDTO
workerStateToDTO app workerId state =
  case state of
    WorkerRuntimeIdle since ->
      pure WorkerStatusDTO
        { workerStatusId = workerId
        , workerStatusState = WorkerStatusIdle
            { workerStatusIdleSince = since
            }
        }
    WorkerRuntimeBusy taskId step startedAt -> do
      mTask <- runSqlPool (get taskId) (appConnPool app)
      let title = fmap taskTitle mTask
          taskNumeric = fromIntegral (fromSqlKey taskId)
      pure WorkerStatusDTO
        { workerStatusId = workerId
        , workerStatusState = WorkerStatusRunning
            { workerStatusTaskId = taskNumeric
            , workerStatusTaskTitle = title
            , workerStatusStep = step
            , workerStatusStartedAt = startedAt
            }
        }

workerMetricsToDTO :: Int -> WorkerMetrics -> WorkerMetricDTO
workerMetricsToDTO workerId WorkerMetrics {..} = WorkerMetricDTO
  { workerMetricId = workerId
  , workerMetricCurrentTaskId = fmap (fromIntegral . fromSqlKey) workerMetricsCurrentTask
  , workerMetricCurrentStep = workerMetricsCurrentStep
  , workerMetricStartedAt = workerMetricsStartedAt
  , workerMetricLastTaskId = fmap (fromIntegral . fromSqlKey) workerMetricsLastTask
  , workerMetricLastStep = workerMetricsLastStep
  , workerMetricLastDurationSeconds = realToFrac <$> workerMetricsLastDuration
  , workerMetricLastSuccess = workerMetricsLastSuccess
  , workerMetricLastError = workerMetricsLastError
  , workerMetricTotalAssignments = workerMetricsAssignments
  , workerMetricTotalBusySeconds = realToFrac workerMetricsBusySeconds
  }

enqueueTask :: App -> TaskId -> IO ()
enqueueTask = enqueueKickoff

enqueueKickoff :: App -> TaskId -> IO ()
enqueueKickoff app@App { appQueue } taskId = do
  resetRetryState app taskId
  clearPausedStatus app taskId
  releaseTaskAssignment app taskId
  enqueueGlobal appQueue (QueueKickoff taskId)

enqueueStep :: App -> TaskId -> WorkflowStep -> IO ()
enqueueStep App { appQueue, appWorkerAssignments } taskId step = do
  assignments <- readTVarIO appWorkerAssignments
  case Map.lookup taskId assignments of
    Just workerId -> enqueueForWorker appQueue workerId (QueueAdvance taskId step)
    Nothing -> enqueueGlobal appQueue (QueueAdvance taskId step)

enqueueTaskStep :: App -> TaskId -> WorkflowStep -> IO ()
enqueueTaskStep = enqueueStep

orchestratorSnapshot :: App -> IO OrchestratorSnapshot
orchestratorSnapshot app@App { appConnPool, appQueue, appWorkerStates, appPausedTasks, appSnoozedTasks, appWorkerMetrics, appWorkerCount } = do
  entities <- runSqlPool (selectList [] [Desc TaskUpdatedAt]) appConnPool
  pausedSet <- readTVarIO appPausedTasks
  snoozedMap <- readTVarIO appSnoozedTasks
  let summaries = fmap (taskSummaryFromEntity pausedSet snoozedMap) entities
  depth <- queueSize appQueue
  currentStates <- readTVarIO appWorkerStates
  currentMetrics <- readTVarIO appWorkerMetrics
  now <- getCurrentTime
  let workerIds = [1 .. appWorkerCount]
      fallbackStates = Map.fromList (map (\workerId -> (workerId, WorkerRuntimeIdle now)) workerIds)
      mergedStates = Map.union currentStates fallbackStates
      fallbackMetrics = Map.fromList (map (\workerId -> (workerId, defaultWorkerMetrics)) workerIds)
      mergedMetrics = Map.union currentMetrics fallbackMetrics
  atomically $ do
    writeTVar appWorkerStates mergedStates
    writeTVar appWorkerMetrics mergedMetrics
  workerDtos <- mapM (uncurry (workerStateToDTO app)) (Map.toList mergedStates)
  let metricDtos = fmap (uncurry workerMetricsToDTO) (Map.toList mergedMetrics)
  pure OrchestratorSnapshot
    { snapshotActiveTasks = summaries
    , snapshotQueueDepth = depth
    , snapshotWorkers = workerDtos
    , snapshotPausedTasks = fmap (fromIntegral . fromSqlKey) (Set.toList pausedSet)
    , snapshotWorkerMetrics = metricDtos
    }

processKickoff :: App -> TaskId -> IO ()
processKickoff app@App { appConnPool } taskId = do
  outcome <- runSqlPool action appConnPool
  case outcome of
    Nothing -> logError $ "Kickoff requested for missing task " <> T.pack (show taskId)
    Just _ -> do
      logEvent app taskId StepIntake "Task scheduled" Nothing
      enqueueStep app taskId StepDesign
  where
    action :: SqlPersistT IO (Maybe TaskRunId)
    action = do
      mTask <- get taskId
      case mTask of
        Nothing -> pure Nothing
        Just _ -> do
          now <- liftIO getCurrentTime
          ord <- nextOrdinal taskId
          runId <- insert TaskRun
            { taskRunTaskId = taskId
            , taskRunOrdinal = ord
            , taskRunCurrentStep = StepIntake
            , taskRunPmSummary = Nothing
            , taskRunCreatedAt = now
            , taskRunUpdatedAt = now
            }
          update taskId
            [ TaskStatus =. TaskStatusDesigning
            , TaskUpdatedAt =. now
            ]
          pure (Just runId)

processStep :: App -> TaskId -> WorkflowStep -> IO ()
processStep app@App { appConnPool } taskId step = do
  mTask <- runSqlPool (get taskId) appConnPool
  case mTask of
    Nothing -> logError $ "Process step requested for missing task " <> taskKeyText taskId
    Just task ->
      if taskStatus task `elem` [TaskStatusDiscarded, TaskStatusCancelled, TaskStatusCompleted]
        then logInfo $ "Skipping step " <> T.pack (show step) <> " for task " <> taskKeyText taskId
        else case step of
          StepDesign -> runDesign app taskId
          StepImplementation -> runImplementation app taskId
          StepSpecVerification -> runSpecVerification app taskId
          StepPmReview -> runPmReview app taskId
          StepQaReview -> runQaReview app taskId
          StepCommit -> runCommit app taskId
          StepPreview -> runPreviewStep app taskId
          StepFinalize -> finalizeTask app taskId
          StepFixIteration -> runFixIteration app taskId
          StepIntake -> pure ()

runDesign :: App -> TaskId -> IO ()
runDesign app@App { appConnPool } taskId = do
  result <- runAgentSession app taskId StepDesign AgentRoleProjectManager "pm_design" Nothing Nothing
  halted <- shouldAbortTask app taskId
  if halted
    then logInfo $ "Design step post-processing skipped for halted task " <> taskKeyText taskId
    else if agentSucceeded result
      then do
        updateRunAndStatus app taskId StepDesign TaskStatusDesigning (arSummary result)
        logEvent app taskId StepDesign "Technical design produced" (Just $ object ["summary" .= arSummary result])
        storeDesignArtifact appConnPool taskId result
        enqueueStep app taskId StepImplementation
      else markBlocked app taskId StepDesign (arSummary result) Nothing

runImplementation :: App -> TaskId -> IO ()
runImplementation app@App { appConnPool, appSettingsVar } taskId = do
  mTask <- runSqlPool (get taskId) appConnPool
  case mTask of
    Nothing -> logError $ "Implementation requested for missing task " <> taskKeyText taskId
    Just task -> do
      let repoRoot = T.unpack (taskRepoRoot task)
          branch = taskBranch task
      let forceReset = isNothing (taskFeatureBranch task)
      worktree <- prepareWorktree app repoRoot branch taskId forceReset
      runSqlPool (update taskId [TaskFeatureBranch =. Just (wtBranch worktree)]) appConnPool
      settings <- readTVarIO appSettingsVar
      let defaultCommand = settingsTestCommand settings
          testCommand = fromMaybe defaultCommand (taskTestCommandOverride task)
      result <- runAgentSession app taskId StepImplementation AgentRoleImplementer "implementer" (Just worktree) (Just testCommand)
      halted <- shouldAbortTask app taskId
      if halted
        then logInfo $ "Implementation step post-processing skipped for halted task " <> taskKeyText taskId
        else if agentSucceeded result
          then do
            resetRetryState app taskId
            updateRunAndStatus app taskId StepImplementation TaskStatusImplementing (arSummary result)
            logEvent app taskId StepImplementation "Implementation agent run completed" (Just $ object ["summary" .= arSummary result])
            logEvent app taskId StepImplementation "Running configured test command after implementation" (Just $ object ["command" .= testCommand])
            tests <- runTestsInWorktree app taskId StepImplementation AgentRoleImplementer worktree testCommand
            storeTestArtifact appConnPool taskId testCommand tests
            case wrExitCode tests of
              ExitSuccess -> do
                logEvent app taskId StepImplementation "Post-implementation tests passed" Nothing
                diffText <- collectDiff worktree
                storeDiffArtifact appConnPool taskId diffText
                enqueueStep app taskId StepSpecVerification
              code -> do
                let payload = object
                      [ "exitCode" .= show code
                      , "command" .= testCommand
                      ]
                    summary = "Test command failed after implementation; see worktree-tests artifact"
                scheduleFixIteration app taskId StepImplementation summary (Just payload)
          else markBlocked app taskId StepImplementation (arSummary result) Nothing

runSpecVerification :: App -> TaskId -> IO ()
runSpecVerification app@App { appConnPool } taskId = do
  mTask <- runSqlPool (get taskId) appConnPool
  case mTask of
    Nothing -> logError $ "Spec verification requested for missing task " <> taskKeyText taskId
    Just task -> do
      let repoRoot = T.unpack (taskRepoRoot task)
      worktree <- locateWorktree repoRoot taskId
      criteria <- runSqlPool (selectList [TaskAcceptanceCriterionTaskId ==. taskId] [Asc TaskAcceptanceCriterionOrdinal]) appConnPool
      evidenceResult <- collectRepoEvidence app taskId repoRoot worktree
      proceed <- processEvidence criteria evidenceResult
      when proceed $ do
        result <- runAgentSession app taskId StepSpecVerification AgentRoleVerifier "verifier" (Just worktree) Nothing
        halted <- shouldAbortTask app taskId
        if halted
          then logInfo $ "Spec verification post-processing skipped for halted task " <> taskKeyText taskId
          else if agentSucceeded result
            then do
              updateRunAndStatus app taskId StepSpecVerification TaskStatusImplementing (arSummary result)
              logEvent app taskId StepSpecVerification "Verifier agent approved implementation" (Just $ object ["summary" .= arSummary result])
              enqueueStep app taskId StepPmReview
            else
              scheduleFixIteration app taskId StepSpecVerification (arSummary result) Nothing
  where
    processEvidence :: [Entity TaskAcceptanceCriterion] -> Either Text EvidenceCapture -> IO Bool
    processEvidence _ (Left err) = do
      logEvent app taskId StepSpecVerification "Automated verifier evidence failed" (Just $ object ["error" .= err])
      scheduleFixIteration app taskId StepSpecVerification "Unable to capture verifier evidence" (Just $ object ["error" .= err])
      pure False
    processEvidence criteria (Right capture) = do
      runSqlPool
        (do
          for_ (ecScreenshotPath capture) $ \path ->
            storeArtifactWithPath taskId
              ( ArtifactScreenshot
              , "verifier-screenshot"
              , Nothing
              , Just (T.pack path)
              )
          storeArtifact taskId
            ( ArtifactVerificationEvidence
            , "verifier-evidence"
            , Just (ecSummary capture)
            )
        )
        appConnPool
      logEvent app taskId StepSpecVerification "Automated verifier evidence captured"
        (Just $ object
          [ "source" .= ecSource capture
          , "metrics" .= ecSummary capture
          ])
      case ecScreenshotPath capture of
        Nothing -> do
          scheduleFixIteration app taskId StepSpecVerification "Screenshot missing from verifier evidence" Nothing
          pure False
        Just shotPath -> do
          automationOutcome <- loadAutomationVerdict (ecSandboxDir capture)
          let storeReport label payload =
                runSqlPool
                  (storeArtifact taskId
                    ( ArtifactVerifierReport
                    , label
                    , Just payload
                    )
                  )
                  appConnPool
              automationReportPayload report =
                object
                  [ "source" .= ("automation" :: Text)
                  , "report" .= avrPayload report
                  ]
              automationFixPayload report =
                object
                  [ "source" .= ("automation" :: Text)
                  , "issues" .= avrIssues report
                  , "summary" .= avrSummary report
                  ]
              geminiReportPayload verdict =
                object
                  [ "source" .= ("gemini" :: Text)
                  , "report" .= object
                      [ "passed" .= gvPassed verdict
                      , "summary" .= gvSummary verdict
                      , "violations" .= gvViolations verdict
                      ]
                  ]
              runGeminiFlow = do
                geminiOutcome <- runGeminiReview (Just shotPath) (ecSummary capture) criteria
                case geminiOutcome of
                  Left geminiErr -> do
                    let payload = object
                          [ "source" .= ("gemini" :: Text)
                          , "error" .= geminiErr
                          ]
                    logEvent app taskId StepSpecVerification "Gemini verifier error" (Just payload)
                    scheduleFixIteration app taskId StepSpecVerification "Gemini verifier failed" (Just payload)
                    pure False
                  Right verdict -> do
                    storeReport "gemini-verdict" (geminiReportPayload verdict)
                    if gvPassed verdict
                      then pure True
                      else do
                        let payload = object
                              [ "source" .= ("gemini" :: Text)
                              , "summary" .= gvSummary verdict
                              , "violations" .= gvViolations verdict
                              ]
                        logEvent app taskId StepSpecVerification "Gemini verifier rejected implementation" (Just payload)
                        scheduleFixIteration app taskId StepSpecVerification (gvSummary verdict)
                          (Just $ object
                            [ "source" .= ("gemini" :: Text)
                            , "violations" .= gvViolations verdict
                            ])
                        pure False
          case automationOutcome of
            Left reportErr -> do
              let payload = object
                    [ "source" .= ("automation" :: Text)
                    , "error" .= reportErr
                    ]
              logEvent app taskId StepSpecVerification "Automation verifier report invalid" (Just payload)
              runGeminiFlow
            Right Nothing ->
              runGeminiFlow
            Right (Just report) -> do
              storeReport "automation-verdict" (automationReportPayload report)
              when (avrPassed report) $
                logEvent app taskId StepSpecVerification "Automation verifier approved implementation"
                  (Just $ object
                    [ "summary" .= avrSummary report
                    , "issues" .= avrIssues report
                    , "source" .= ("automation" :: Text)
                    ])
              when (not (avrPassed report)) $
                logEvent app taskId StepSpecVerification "Automation verifier rejected implementation"
                  (Just $ object
                    [ "summary" .= avrSummary report
                    , "issues" .= avrIssues report
                    , "source" .= ("automation" :: Text)
                    ])
              runGeminiFlow

collectRepoEvidence :: App -> TaskId -> FilePath -> WorktreeContext -> IO (Either Text EvidenceCapture)
collectRepoEvidence app@App { appRepoProfiles } taskId repoRoot worktree = do
  mProfile <- lookupRepoProfile appRepoProfiles repoRoot
  case mProfile of
    Nothing -> collectDemoEvidence defaultEvidenceOptions worktree
    Just profile@RepoProfile { rpName } -> do
      logEvent app taskId StepSpecVerification "Applying repo profile before verification"
        (withOrigin "system" (Just $ object ["profile" .= rpName]))
      setupResult <- runRepoProfileSetup profile
      case setupResult of
        Left err -> do
          logEvent app taskId StepSpecVerification "Repo profile setup failed; falling back to demo evidence"
            (Just $ object ["profile" .= rpName, "error" .= err])
          collectDemoEvidence (evidenceOptionsFromProfile profile) worktree
        Right () ->
          collectDemoEvidence (evidenceOptionsFromProfile profile) worktree

evidenceOptionsFromProfile :: RepoProfile -> EvidenceOptions
evidenceOptionsFromProfile RepoProfile { rpSandboxOverride, rpCaptureCommand, rpEnv } =
  defaultEvidenceOptions
    { eoSandboxOverride = rpSandboxOverride
    , eoCaptureCommand = rpCaptureCommand
    , eoEnv = ensureDefault "VERIFIER_DISABLE_PLAYWRIGHT" "0" (Map.toList rpEnv)
    }
  where
    ensureDefault key value env =
      if any ((== key) . fst) env
        then env
        else (key, value) : env

data AutomationReport = AutomationReport
  { avrPayload :: Value
  , avrPassed :: Bool
  , avrSummary :: Text
  , avrIssues :: [Text]
  }

data AutomationVerdict = AutomationVerdict
  { avPassed :: Bool
  , avSummary :: Text
  , avIssues :: [Text]
  }

instance FromJSON AutomationVerdict where
  parseJSON = withObject "AutomationVerdict" $ \obj -> do
    passed <- obj .: "passed"
    summary <- obj .: "summary"
    issues <- fromMaybe [] <$> obj .:? "issues"
    pure AutomationVerdict
      { avPassed = passed
      , avSummary = summary
      , avIssues = issues
      }

loadAutomationVerdict :: FilePath -> IO (Either Text (Maybe AutomationReport))
loadAutomationVerdict sandboxDir = do
  let reportPath = sandboxDir </> "automation-report.json"
  reportExists <- doesFileExist reportPath
  if not reportExists
    then pure (Right Nothing)
    else do
      bytesResult <- try @SomeException (BL.readFile reportPath)
      case bytesResult of
        Left err ->
          pure . Left $ "Unable to read automation report: " <> T.pack (displayException err)
        Right bytes ->
          case eitherDecode bytes :: Either String Value of
            Left err ->
              pure . Left $ "Invalid automation report JSON: " <> T.pack err
            Right value ->
              case parseEither parseJSON value of
                Left err ->
                  pure . Left $ "Automation report missing required fields: " <> T.pack err
                Right verdict ->
                  pure . Right . Just $
                    AutomationReport
                      { avrPayload = value
                      , avrPassed = avPassed verdict
                      , avrSummary = avSummary verdict
                      , avrIssues = avIssues verdict
                      }

runPmReview :: App -> TaskId -> IO ()
runPmReview app taskId = do
  result <- runAgentSession app taskId StepPmReview AgentRoleProjectManager "pm_design" Nothing Nothing
  halted <- shouldAbortTask app taskId
  if halted
    then logInfo $ "PM review post-processing skipped for halted task " <> taskKeyText taskId
    else if agentSucceeded result
      then do
        updateRunAndStatus app taskId StepPmReview TaskStatusReviewing (arSummary result)
        logEvent app taskId StepPmReview "Project manager review complete" (Just $ object ["summary" .= arSummary result])
        enqueueStep app taskId StepCommit
      else
        scheduleFixIteration app taskId StepPmReview (arSummary result) Nothing

runQaReview :: App -> TaskId -> IO ()
runQaReview app taskId = do
  logEvent app taskId StepQaReview "QA step skipped (manual QA not required)" Nothing
  enqueueStep app taskId StepCommit

runFixIteration :: App -> TaskId -> IO ()
runFixIteration app taskId = do
  updateRunAndStatus app taskId StepFixIteration TaskStatusImplementing "Fix iteration scheduled"
  logEvent app taskId StepFixIteration "Re-running implementation to address QA feedback" Nothing
  enqueueStep app taskId StepImplementation

cancelTask :: App -> TaskId -> IO ()
cancelTask app@App { appConnPool } taskId = do
  mTask <- runSqlPool (get taskId) appConnPool
  case mTask of
    Nothing -> logError $ "Cancel requested for missing task " <> taskKeyText taskId
    Just task -> do
      mRuntime <- popAgentRuntime app taskId
      for_ mRuntime $ \runtime -> do
        logInfo $ "Terminating active agent for task " <> taskKeyText taskId <> " due to manual cancellation"
        terminateRuntime runtime
      now <- getCurrentTime
      runSqlPool
        (update taskId
          [ TaskStatus =. TaskStatusCancelled
          , TaskUpdatedAt =. now
          ])
        appConnPool
      logEvent app taskId StepFinalize "Task cancelled by human" (payloadFromRuntime <$> mRuntime)
      teardownPreview appConnPool taskId
      let repoRoot = taskRepoRoot task
      unless (T.null repoRoot) $ do
        worktree <- locateWorktree (T.unpack repoRoot) taskId
        cleanupWorktree worktree
      releaseTaskAssignment app taskId
      resetRetryState app taskId
      clearPausedStatus app taskId

payloadFromRuntime :: AgentRuntime -> Value
payloadFromRuntime AgentRuntime { agentRole, agentStep } =
  object
    [ "role" .= agentRole
    , "step" .= agentStep
    , "reason" .= ("Manual cancellation" :: Text)
    ]

performCommitAndPush :: WorktreeContext -> Text -> Text -> TaskId -> IO (Either Text CommitLogs)
performCommitAndPush WorktreeContext { wtRoot } branch title taskId = do
  statusRes <- runGit wtRoot ["status", "--porcelain"]
  addRes <- runGit wtRoot ["add", "--all"]
  if not (commandSucceeded addRes)
    then pure . Left $ "git add failed: " <> summarizeFailure addRes
    else do
      commitResEither <-
        if cleanWorkingTree statusRes
          then pure (Right Nothing)
          else do
            commitRes <- runGit wtRoot ["commit", "--message", T.unpack (commitMessage title taskId)]
            pure $ if commandSucceeded commitRes
              then Right (Just commitRes)
              else Left ("git commit failed: " <> summarizeFailure commitRes)
      case commitResEither of
        Left err -> pure (Left err)
        Right commitRes -> do
          pushRes <- runGit wtRoot ["push", "--set-upstream", "origin", T.unpack branch]
          if commandSucceeded pushRes
            then pure $ Right CommitLogs
              { clStatus = statusRes
              , clAdd = addRes
              , clCommit = commitRes
              , clPush = pushRes
              }
            else pure . Left $ "git push failed: " <> summarizeFailure pushRes
  where
    cleanWorkingTree CommandResult { crStdout } = T.null (T.strip crStdout)

commitMessage :: Text -> TaskId -> Text
commitMessage title taskId =
  let taskLabel = T.pack ("[Task #" <> show (fromSqlKey taskId) <> "] ")
      sanitized = T.unwords (T.words title)
   in taskLabel <> T.take 160 sanitized

runCommit :: App -> TaskId -> IO ()
runCommit app@App { appConnPool } taskId = do
  mTask <- runSqlPool (get taskId) appConnPool
  case mTask of
    Nothing -> logError $ "Commit requested for missing task " <> taskKeyText taskId
    Just task -> do
      let repoRoot = T.unpack (taskRepoRoot task)
          titleText = taskTitle task
      worktree <- locateWorktree repoRoot taskId
      let featureBranch = fromMaybe (wtBranch worktree) (taskFeatureBranch task)
      when (isNothing (taskFeatureBranch task)) $
        runSqlPool (update taskId [TaskFeatureBranch =. Just featureBranch]) appConnPool
      outcome <- performCommitAndPush worktree featureBranch titleText taskId
      case outcome of
        Left err -> markBlocked app taskId StepCommit err Nothing
        Right logs -> do
          let summary = "Committed and pushed to branch " <> featureBranch
              payload = Just $ object
                [ "branch" .= featureBranch
                ]
          updateRunAndStatus app taskId StepCommit TaskStatusReviewing summary
          logEvent app taskId StepCommit "Commit and push completed" payload
          storeCommitArtifact appConnPool taskId logs
          enqueueStep app taskId StepPreview

runPreviewStep :: App -> TaskId -> IO ()
runPreviewStep app@App { appConnPool } taskId = do
  mTask <- runSqlPool (get taskId) appConnPool
  case mTask of
    Nothing -> logError $ "Preview requested for missing task " <> taskKeyText taskId
    Just _ -> do
      let url = previewUrlForTask taskId
      teardownPreview appConnPool taskId
      now <- getCurrentTime
      runSqlPool
        (update taskId
          [ TaskPreviewUrl =. Just url
          , TaskPreviewStatus =. PreviewOffline
          , TaskUpdatedAt =. now
          ])
        appConnPool
      updateRunAndStatus app taskId StepPreview TaskStatusReviewing "Preview pending manual launch"
      logEvent app taskId StepPreview "Preview ready; launch manually from UI" (Just $ object ["url" .= url])

finalizeTask :: App -> TaskId -> IO ()
finalizeTask app@App { appConnPool } taskId = do
  now <- getCurrentTime
  runSqlPool
    (do
      updateLatestRun taskId StepFinalize Nothing
      update taskId
        [ TaskStatus =. TaskStatusCompleted
        , TaskUpdatedAt =. now
        ]
    )
    appConnPool
  logEvent app taskId StepFinalize "Task marked completed" Nothing
  resetRetryState app taskId
  clearPausedStatus app taskId

runAgentSession :: App -> TaskId -> WorkflowStep -> AgentRole -> Text -> Maybe WorktreeContext -> Maybe Text -> IO AgentResult
runAgentSession app@App { appConnPool, appAgentRegistry, appHeartbeatVar } taskId step role promptKey mWorktree mTestCommand = do
  template <- runSqlPool (selectFirst [PromptTemplateKey ==. promptKey] []) appConnPool
  mTask <- runSqlPool (get taskId) appConnPool
  let instructions = instructionsFor role step mTestCommand
      basePrompt = maybe "" (promptTemplateContent . entityVal) template
  hints <- consumeRetryHints app taskId step
  let hintInstructions = formatRetryHints hints
      mergedInstructions = combineInstructions instructions hintInstructions
      extraInstructions = maybe "" (\txt -> "\n\nAdditional Instructions:\n" <> txt) mergedInstructions
      augmentedPrompt = basePrompt <> extraInstructions
  context <- runSqlPool (buildAgentContext taskId step mWorktree mergedInstructions) appConnPool
  workingDir <- resolveWorkingDir mWorktree mTask
  touchHeartbeat app taskId
  let publishStream = broadcastAgentLog app taskId step role
      gateway = defaultGateway
      registerHandle ph = do
        now <- getCurrentTime
        let runtime = AgentRuntime
              { agentHandle = ph
              , agentRole = role
              , agentStep = step
              , agentStartedAt = now
              }
        atomically $ do
          modifyTVar' appAgentRegistry (Map.insert taskId runtime)
          modifyTVar' appHeartbeatVar (Map.insert taskId now)
      unregisterHandle = atomically $ do
        modifyTVar' appAgentRegistry (Map.delete taskId)
        modifyTVar' appHeartbeatVar (Map.delete taskId)
      invocation = AgentInvocation
        { aiTaskId = taskId
        , aiRole = role
        , aiPromptKey = promptKey
        , aiPromptContent = augmentedPrompt
        , aiContext = context
        , aiWorkingDir = workingDir
        , aiEnvironment = [("RUST_LOG", "error")]
        , aiPublish = publishStream
        , aiOnStart = registerHandle
        , aiOnComplete = unregisterHandle
        }
  result <- runAgentWithRetries app taskId step role gateway invocation
  now <- getCurrentTime
  runSqlPool
    (do
      runId <- ensureLatestRun taskId
      _ <- insert AgentSession
        { agentSessionTaskRunId = runId
        , agentSessionRole = role
        , agentSessionPromptKey = promptKey
        , agentSessionStatus = arStatus result
        , agentSessionLastError = Nothing
        , agentSessionLogPath = Nothing
        , agentSessionCreatedAt = now
        , agentSessionUpdatedAt = now
        }
      mapM_ (storeArtifact taskId) (arArtifacts result)
    )
    appConnPool
  pure result

runAgentWithRetries :: App -> TaskId -> WorkflowStep -> AgentRole -> AgentGateway -> AgentInvocation -> IO AgentResult
runAgentWithRetries app taskId step _ AgentGateway { runAgent } invocation = go (1 :: Int)
  where
    maxAttempts = 3
    go attempt = do
      result <- runAgent invocation
      if arStatus result == AgentSessionSucceeded || attempt >= maxAttempts || not (shouldRetry result)
        then pure result
        else do
          let waitSeconds = attempt * 5
              payload = Just $ object
                [ "attempt" .= attempt
                , "maxAttempts" .= maxAttempts
                , "reason" .= arSummary result
                ]
              msg = "Agent run failed; retrying in " <> showText waitSeconds <> "s (attempt " <> showText (attempt + 1) <> "/" <> showText maxAttempts <> ")"
          logEvent app taskId step msg payload
          touchHeartbeat app taskId
          threadDelay (waitSeconds * 1000000)
          go (attempt + 1)

    shouldRetry AgentResult { arStatus = AgentSessionErrored, arSummary, arStderr } =
      let combined = T.toLower (arSummary <> "\n" <> arStderr)
       in any (`T.isInfixOf` combined)
            [ "failed to decode response"
            , "rate limit"
            , "temporarily unavailable"
            , "timeout"
            , "connection reset"
            , "codex invocation failed"
            ]
    shouldRetry _ = False

storeArtifact :: TaskId -> (ArtifactKind, Text, Maybe Value) -> SqlPersistT IO ()
storeArtifact taskId (kind, label, payload) =
  storeArtifactWithPath taskId (kind, label, payload, Nothing)

storeArtifactWithPath :: TaskId -> (ArtifactKind, Text, Maybe Value, Maybe Text) -> SqlPersistT IO ()
storeArtifactWithPath taskId (kind, label, payload, mPath) = do
  now <- liftIO getCurrentTime
  insert_ Artifact
    { artifactTaskId = taskId
    , artifactKind = kind
    , artifactLabel = label
    , artifactContent = payload
    , artifactPath = mPath
    , artifactCreatedAt = now
    }

storeTestArtifact :: ConnectionPool -> TaskId -> Text -> WorktreeResult -> IO ()
storeTestArtifact pool taskId command WorktreeResult { wrExitCode, wrStdout, wrStderr } =
  runSqlPool
    (storeArtifact taskId
      ( ArtifactTestLog
      , "worktree-tests"
      , Just $ object
          [ "exitCode" .= show wrExitCode
          , "stdout" .= wrStdout
          , "stderr" .= wrStderr
          , "command" .= command
          ]
      )
    )
    pool

storeDiffArtifact :: ConnectionPool -> TaskId -> Text -> IO ()
storeDiffArtifact pool taskId diffText =
  runSqlPool
    (storeArtifact taskId
      ( ArtifactDiff
      , "worktree-diff"
      , Just $ object ["diff" .= diffText]
      )
    )
    pool

agentSucceeded :: AgentResult -> Bool
agentSucceeded AgentResult { arStatus = AgentSessionSucceeded } = True
agentSucceeded _ = False

shouldAbortTask :: App -> TaskId -> IO Bool
shouldAbortTask App { appConnPool } taskId =
  runSqlPool
    (do
      mTask <- get taskId
      pure $ maybe False (taskStatusHalting . taskStatus) mTask
    )
    appConnPool

taskStatusHalting :: TaskStatus -> Bool
taskStatusHalting status = status `elem`
  [ TaskStatusCancelled
  , TaskStatusDiscarded
  , TaskStatusCompleted
  ]

popAgentRuntime :: App -> TaskId -> IO (Maybe AgentRuntime)
popAgentRuntime App { appAgentRegistry, appHeartbeatVar } taskId =
  atomically $ do
    registry <- readTVar appAgentRegistry
    let runtime = Map.lookup taskId registry
    modifyTVar' appAgentRegistry (Map.delete taskId)
    modifyTVar' appHeartbeatVar (Map.delete taskId)
    pure runtime

terminateRuntime :: AgentRuntime -> IO ()
terminateRuntime AgentRuntime { agentHandle } =
  void $ try @SomeException (terminateProcess agentHandle)

inactivityMonitor :: App -> IO ()
inactivityMonitor app@App { appAgentRegistry, appHeartbeatVar, appSettingsVar } = forever $ do
  threadDelay (30 * 1000000)
  settings <- readTVarIO appSettingsVar
  let configuredMinutes = max 1 (settingsInactivityMinutes settings)
      threshold :: NominalDiffTime
      threshold = realToFrac (configuredMinutes * 60)
  registrySnapshot <- readTVarIO appAgentRegistry
  heartbeatSnapshot <- readTVarIO appHeartbeatVar
  now <- getCurrentTime
  for_ (Map.toList registrySnapshot) $ \(taskId, runtime) -> do
    let age = maybe (diffUTCTime now (agentStartedAt runtime)) (diffUTCTime now) (Map.lookup taskId heartbeatSnapshot)
    when (age > threshold) $
      handleInactivityTimeout app taskId runtime age configuredMinutes

snoozeMonitor :: App -> IO ()
snoozeMonitor app@App { appSnoozedTasks } = forever $ do
  threadDelay (15 * 1000000)
  now <- getCurrentTime
  due <- atomically $ do
    entries <- readTVar appSnoozedTasks
    let (ready, pending) = Map.partition (<= now) entries
    writeTVar appSnoozedTasks pending
    pure (Map.toList ready)
  for_ due $ \(taskId, resumeAt) -> do
    logEvent app taskId StepFixIteration "Snooze elapsed; resuming automatically"
      (withOrigin "system" (Just $ object ["resumeAt" .= resumeAt]))
    _ <- resumeTaskAfterMessage app taskId "Snooze auto-resume" Nothing Nothing True "system"
    pure ()

handleInactivityTimeout :: App -> TaskId -> AgentRuntime -> NominalDiffTime -> Int -> IO ()
handleInactivityTimeout app taskId _ age minutes = do
  mRuntime <- popAgentRuntime app taskId
  case mRuntime of
    Nothing -> pure ()
    Just activeRuntime -> do
      logInfo $ "Agent timeout detected for task " <> taskKeyText taskId <> " during " <> T.pack (show (agentStep activeRuntime))
      terminateRuntime activeRuntime
      let elapsedSeconds :: Double
          elapsedSeconds = realToFrac age
          payload = Just $ object
            [ "role" .= agentRole activeRuntime
            , "step" .= agentStep activeRuntime
            , "timeoutMinutes" .= minutes
            , "elapsedSeconds" .= elapsedSeconds
            ]
      logEvent app taskId (agentStep activeRuntime) "Agent heartbeat timed out; restarting step" (withOrigin "system" payload)
      halted <- shouldAbortTask app taskId
      when (not halted) $
        let timeoutHint =
              "Previous run of "
                <> T.pack (show (agentStep activeRuntime))
                <> " timed out after "
                <> showText minutes
                <> " minutes. Consider optimizing the command or breaking the work into smaller changes."
         in do
          addRetryHint app taskId (agentStep activeRuntime) timeoutHint
          enqueueStep app taskId (agentStep activeRuntime)

markBlocked :: App -> TaskId -> WorkflowStep -> Text -> Maybe Value -> IO ()
markBlocked app taskId step summary payload = do
  updateRunAndStatus app taskId step TaskStatusBlocked summary
  logEvent app taskId step summary (withOrigin "system" payload)

maxAutoRetries :: Int
maxAutoRetries = 3

scheduleFixIteration :: App -> TaskId -> WorkflowStep -> Text -> Maybe Value -> IO ()
scheduleFixIteration app taskId step summary payload = do
  let hint = buildFixIterationHint step summary payload
  unless (T.null (T.strip hint)) $
    addRetryHint app taskId StepImplementation hint
  newCount <- incrementRetryCounter app taskId
  if newCount > maxAutoRetries
    then do
      case escalationPlanFor step of
        Just (escalateStep, escalateStatus, escalateRole) -> do
          resetRetryCounter app taskId
          let escalationSummary =
                "Automatic retries exhausted; escalating to "
                  <> showText escalateStep
                  <> " for "
                  <> showText escalateRole
          updateRunAndStatus app taskId escalateStep escalateStatus escalationSummary
          let hintMessage = T.unlines
                [ "Automatic escalation after repeated failures."
                , "Previous step: " <> showText step
                , "Latest summary: " <> summary
                ]
          addRetryHint app taskId (hintStepFor escalateStep) hintMessage
          logEvent app taskId escalateStep escalationSummary (withOrigin "system" payload)
          enqueueStep app taskId escalateStep
        Nothing -> do
          let escalationSummary =
                "Automatic retries exhausted after "
                  <> showText maxAutoRetries
                  <> " attempts. Last failure: "
                  <> summary
          markBlocked app taskId step escalationSummary payload
          releaseTaskAssignment app taskId
    else do
      updateRunAndStatus app taskId step TaskStatusImplementing summary
      logEvent app taskId step summary (withOrigin "system" payload)
      enqueueStep app taskId StepFixIteration

forceRetry :: App -> TaskId -> Maybe Text -> IO (Either Text WorkflowStep)
forceRetry app@App { appConnPool } taskId mInstructions = do
  mRun <- runSqlPool (selectFirst [TaskRunTaskId ==. taskId] [Desc TaskRunOrdinal]) appConnPool
  case mRun of
    Nothing -> pure (Left "Task has no recorded runs yet")
    Just (Entity _ run) -> do
      let currentStep = Models.taskRunCurrentStep run
          hintStep = if currentStep == StepFixIteration then StepImplementation else currentStep
          trimmed = fmap T.strip mInstructions
      mRuntime <- popAgentRuntime app taskId
      for_ mRuntime terminateRuntime
      for_ trimmed $ \txt ->
        unless (T.null txt) $
          addRetryHint app taskId hintStep ("Manual retry instruction: " <> txt)
      resetRetryCounter app taskId
      let payloadFields =
            maybe [] (\txt -> if T.null txt then [] else ["instructions" .= txt]) trimmed
              <> maybe []
                    (\runtime ->
                      [ "interruptedStep" .= agentStep runtime
                      , "interruptedRole" .= agentRole runtime
                      ])
                    mRuntime
          payload = if null payloadFields then Nothing else Just (object payloadFields)
      logEvent app taskId currentStep "Manual force retry requested" (withOrigin "human" payload)
      resumeTask app taskId
      enqueueTaskStep app taskId currentStep
      pure (Right currentStep)

reassignTask :: App -> TaskId -> IO (Either Text WorkflowStep)
reassignTask app@App { appConnPool } taskId = do
  mRun <- runSqlPool (selectFirst [TaskRunTaskId ==. taskId] [Desc TaskRunOrdinal]) appConnPool
  case mRun of
    Nothing -> pure (Left "Task has no recorded runs yet")
    Just (Entity _ run) -> do
      let currentStep = Models.taskRunCurrentStep run
      mRuntime <- popAgentRuntime app taskId
      for_ mRuntime terminateRuntime
      releaseTaskAssignment app taskId
      let payload = case mRuntime of
            Nothing -> Nothing
            Just runtime -> Just $ object
              [ "interruptedStep" .= agentStep runtime
              , "interruptedRole" .= agentRole runtime
              ]
      logEvent app taskId currentStep "Worker reassigned by human" (withOrigin "human" payload)
      resumeTask app taskId
      enqueueTaskStep app taskId currentStep
      pure (Right currentStep)

redirectWorker :: App -> TaskId -> TaskId -> IO (Either Text TaskRedirectResponse)
redirectWorker app@App { appWorkerAssignments, appQueue, appConnPool } sourceTask targetTask
  | sourceTask == targetTask = pure (Left "Source and target tasks must differ")
  | otherwise = do
      assignments <- readTVarIO appWorkerAssignments
      case Map.lookup sourceTask assignments of
        Nothing -> pure (Left "No worker currently assigned to this task")
        Just workerId -> do
          case Map.lookup targetTask assignments of
            Just other | other /= workerId ->
              pure (Left "Target task is already owned by a different worker")
            _ -> do
              sourceOutcome <- reassignTask app sourceTask
              case sourceOutcome of
                Left err -> pure (Left err)
                Right sourceStep -> do
                  targetPlan <- runSqlPool (determineTargetMessage targetTask) appConnPool
                  case targetPlan of
                    Left err -> pure (Left err)
                    Right (queueMsg, targetStep) -> do
                      resumeTask app targetTask
                      enqueueForWorker appQueue workerId queueMsg
                      let
                        payloadValue = object
                          [ "workerId" .= workerId
                          , "sourceTask" .= fromSqlKey sourceTask
                          , "targetTask" .= fromSqlKey targetTask
                          , "step" .= targetStep
                          ]
                        payload = Just payloadValue
                      logEvent app targetTask targetStep "Worker redirected to this task" (withOrigin "human" payload)
                      let response = TaskRedirectResponse
                            { taskRedirectRequeuedStep = sourceStep
                            , taskRedirectTargetTaskId = Just (fromIntegral (fromSqlKey targetTask))
                            , taskRedirectTargetStep = Just targetStep
                            , taskRedirectWorkerId = Just workerId
                            }
                      pure (Right response)

determineTargetMessage :: TaskId -> SqlPersistT IO (Either Text (QueueMessage, WorkflowStep))
determineTargetMessage taskId = do
  mTask <- get taskId
  case mTask of
    Nothing -> pure (Left "Target task not found")
    Just task
      | taskStatusHalting (taskStatus task) ->
          pure (Left "Target task is already finished")
      | otherwise -> do
          runId <- ensureLatestRun taskId
          mRun <- get runId
          let step = maybe StepDesign Models.taskRunCurrentStep mRun
              queueMsg = case step of
                StepIntake -> QueueKickoff taskId
                _ -> QueueAdvance taskId step
          pure (Right (queueMsg, step))

addRetryHint :: App -> TaskId -> WorkflowStep -> Text -> IO ()
addRetryHint App { appRetryHints } taskId step hint =
  atomically $ modifyTVar' appRetryHints $ \hintsMap ->
    let updatedStepMap = case Map.lookup taskId hintsMap of
          Nothing -> Map.singleton step [hint]
          Just stepMap -> Map.insertWith (++) step [hint] stepMap
    in Map.insert taskId updatedStepMap hintsMap

consumeRetryHints :: App -> TaskId -> WorkflowStep -> IO [Text]
consumeRetryHints App { appRetryHints } taskId step =
  atomically $ do
    hintsMap <- readTVar appRetryHints
    let (hints, newMap) = case Map.lookup taskId hintsMap of
          Nothing -> ([], hintsMap)
          Just stepMap ->
            let (stepHints, remainingStepMap) =
                  case Map.lookup step stepMap of
                    Nothing -> ([], stepMap)
                    Just hs -> (hs, Map.delete step stepMap)
                updatedHintsMap =
                  if Map.null remainingStepMap
                    then Map.delete taskId hintsMap
                    else Map.insert taskId remainingStepMap hintsMap
            in (stepHints, updatedHintsMap)
    writeTVar appRetryHints newMap
    pure hints

incrementRetryCounter :: App -> TaskId -> IO Int
incrementRetryCounter App { appRetryCounters } taskId =
  atomically $ do
    counters <- readTVar appRetryCounters
    let newCount = maybe 1 (+ 1) (Map.lookup taskId counters)
    writeTVar appRetryCounters (Map.insert taskId newCount counters)
    pure newCount

resetRetryCounter :: App -> TaskId -> IO ()
resetRetryCounter App { appRetryCounters } taskId =
  atomically $ modifyTVar' appRetryCounters (Map.delete taskId)

clearRetryHints :: App -> TaskId -> IO ()
clearRetryHints App { appRetryHints } taskId =
  atomically $ modifyTVar' appRetryHints (Map.delete taskId)

resetRetryState :: App -> TaskId -> IO ()
resetRetryState app taskId = do
  resetRetryCounter app taskId
  clearRetryHints app taskId

clearPausedStatus :: App -> TaskId -> IO ()
clearPausedStatus App { appPausedTasks, appSnoozedTasks } taskId =
  atomically $ do
    modifyTVar' appPausedTasks (Set.delete taskId)
    modifyTVar' appSnoozedTasks (Map.delete taskId)

formatRetryHints :: [Text] -> Maybe Text
formatRetryHints [] = Nothing
formatRetryHints hints =
  let header = "Address the following issues before continuing:"
      bullets = fmap ("- " <>) hints
   in Just (T.unlines (header : bullets))

combineInstructions :: Maybe Text -> Maybe Text -> Maybe Text
combineInstructions base extra =
  case (base, extra) of
    (Nothing, Nothing) -> Nothing
    (Just b, Nothing) -> Just b
    (Nothing, Just e) -> Just e
    (Just b, Just e) -> Just (b <> "\n\n" <> e)

withOrigin :: Text -> Maybe Value -> Maybe Value
withOrigin origin maybeValue =
  let originKey = Key.fromText "origin"
  in Just $ case maybeValue of
    Nothing -> object ["origin" .= origin]
    Just (Object obj) -> Object (KeyMap.insert originKey (String origin) obj)
    Just other -> object ["origin" .= origin, "payload" .= other]

buildFixIterationHint :: WorkflowStep -> Text -> Maybe Value -> Text
buildFixIterationHint step summary payload =
  case step of
    StepPmReview ->
      summary <> maybe "" formatTestPayload payload
    StepImplementation ->
      summary <> maybe "" formatTestPayload payload
    StepSpecVerification -> summary
    _ -> summary
  where
    formatTestPayload (Object obj) =
      let exitCodeTxt = extractTextField "exitCode" obj
          commandTxt = extractTextField "command" obj
          exitSnippet = maybe "" (\code -> " (exit code " <> code <> ")") exitCodeTxt
          commandSnippet = maybe "" (\cmd -> " for command `" <> cmd <> "`") commandTxt
       in " Test command" <> commandSnippet <> exitSnippet <> ". Review failing output in the `worktree-tests` artifact."
    formatTestPayload _ = ""

    extractTextField key obj =
      case KeyMap.lookup (Key.fromText key) obj of
        Just (String txt) -> Just txt
        Just (Number n) -> Just (T.pack (show n))
        _ -> Nothing

resumeTaskAfterMessage :: App -> TaskId -> Text -> Maybe AgentRole -> Maybe WorkflowStep -> Bool -> Text -> IO Bool
resumeTaskAfterMessage app@App { appConnPool } taskId humanMessage mRole mRequestedStep autoResume originLabel = do
  mInterrupted <- popAgentRuntime app taskId
  for_ mInterrupted $ \runtime -> do
    terminateRuntime runtime
    let snippet = T.take 200 humanMessage
    let interruptionMessage = if originLabel == "system" then "Agent interrupted by automation" else "Agent interrupted by human message"
    logEvent app taskId (agentStep runtime)
      interruptionMessage
      (withOrigin originLabel $ Just $ object
        [ "role" .= agentRole runtime
        , "step" .= agentStep runtime
        , "snippet" .= snippet
        , "autoResume" .= autoResume
        , "requestedStep" .= fmap showText mRequestedStep
        , "requestedRole" .= fmap showText mRole
        ])

  mTask <- runSqlPool (get taskId) appConnPool
  case mTask of
    Nothing -> pure False
    Just task ->
      if taskStatusHalting (taskStatus task)
        then pure False
        else do
          runId <- runSqlPool (ensureLatestRun taskId) appConnPool
          mRun <- runSqlPool (get runId) appConnPool
          let trimmed = T.strip humanMessage
              hintedStep = case mRequestedStep <|> defaultStepForRole mRole of
                Just step -> step
                Nothing -> StepFixIteration
              hintTarget = hintStepFor hintedStep
          unless (T.null trimmed) $
            addRetryHint app taskId hintTarget ("Human guidance: " <> T.take 200 trimmed)
          when autoResume $ do
            resumeTask app taskId
            cancelTaskSnooze app taskId
          let currentStep = case mInterrupted of
                              Just runtime -> agentStep runtime
                              Nothing -> maybe StepImplementation Models.taskRunCurrentStep mRun
              manualPlan = manualResumePlan (mRequestedStep <|> defaultStepForRole mRole)
              planToUse = manualPlan <|> resumePlanFor currentStep
          case (planToUse, mRun, autoResume) of
            (Just (stepToQueue, nextStatus), Just _, True) -> do
              now <- getCurrentTime
              runSqlPool
                (do
                  update taskId
                    [ TaskStatus =. nextStatus
                    , TaskUpdatedAt =. now
                    ]
                  update runId
                    [ TaskRunCurrentStep =. stepToQueue
                    , TaskRunUpdatedAt =. now
                    ]
                )
                appConnPool
              let snippet = T.take 200 humanMessage
                  payload = Just $ object
                    [ "reason" .= ("Human message" :: Text)
                    , "snippet" .= snippet
                    , "scheduledStep" .= stepToQueue
                    , "requestedStep" .= fmap showText mRequestedStep
                    , "requestedRole" .= fmap showText mRole
                    ]
              let resumeMessage = if originLabel == "system" then "Automatic resume triggered" else "Manual retry triggered after human message"
              logEvent app taskId stepToQueue resumeMessage (withOrigin originLabel payload)
              resetRetryCounter app taskId
              enqueueStep app taskId stepToQueue
              pure True
            _ -> pure False

resumePlanFor :: WorkflowStep -> Maybe (WorkflowStep, TaskStatus)
resumePlanFor StepIntake = Just (StepDesign, TaskStatusDesigning)
resumePlanFor StepDesign = Just (StepDesign, TaskStatusDesigning)
resumePlanFor StepImplementation = Just (StepFixIteration, TaskStatusImplementing)
resumePlanFor StepSpecVerification = Just (StepFixIteration, TaskStatusImplementing)
resumePlanFor StepPmReview = Just (StepFixIteration, TaskStatusImplementing)
resumePlanFor StepQaReview = Just (StepFixIteration, TaskStatusImplementing)
resumePlanFor StepFixIteration = Just (StepImplementation, TaskStatusImplementing)
resumePlanFor StepCommit = Just (StepCommit, TaskStatusReviewing)
resumePlanFor StepPreview = Just (StepPreview, TaskStatusReviewing)
resumePlanFor _ = Nothing

manualResumePlan :: Maybe WorkflowStep -> Maybe (WorkflowStep, TaskStatus)
manualResumePlan Nothing = Nothing
manualResumePlan (Just StepImplementation) = Just (StepImplementation, TaskStatusImplementing)
manualResumePlan (Just StepFixIteration) = Just (StepFixIteration, TaskStatusImplementing)
manualResumePlan (Just StepPmReview) = Just (StepPmReview, TaskStatusReviewing)
manualResumePlan (Just StepSpecVerification) = Just (StepSpecVerification, TaskStatusImplementing)
manualResumePlan (Just StepQaReview) = Just (StepQaReview, TaskStatusQa)
manualResumePlan (Just StepCommit) = Just (StepCommit, TaskStatusReviewing)
manualResumePlan (Just StepPreview) = Just (StepPreview, TaskStatusReviewing)
manualResumePlan (Just StepFinalize) = Just (StepFinalize, TaskStatusReviewing)
manualResumePlan (Just StepDesign) = Just (StepDesign, TaskStatusDesigning)
manualResumePlan (Just StepIntake) = Just (StepDesign, TaskStatusDesigning)
manualResumePlan _ = Nothing

defaultStepForRole :: Maybe AgentRole -> Maybe WorkflowStep
defaultStepForRole (Just AgentRoleProjectManager) = Just StepPmReview
defaultStepForRole (Just AgentRoleImplementer) = Just StepImplementation
defaultStepForRole (Just AgentRoleQa) = Just StepQaReview
defaultStepForRole (Just AgentRoleVerifier) = Just StepSpecVerification
defaultStepForRole _ = Nothing

hintStepFor :: WorkflowStep -> WorkflowStep
hintStepFor StepFixIteration = StepImplementation
hintStepFor step = step

escalationPlanFor :: WorkflowStep -> Maybe (WorkflowStep, TaskStatus, AgentRole)
escalationPlanFor StepImplementation = Just (StepPmReview, TaskStatusReviewing, AgentRoleProjectManager)
escalationPlanFor StepFixIteration = Just (StepPmReview, TaskStatusReviewing, AgentRoleProjectManager)
escalationPlanFor StepSpecVerification = Just (StepPmReview, TaskStatusReviewing, AgentRoleProjectManager)
escalationPlanFor StepPmReview = Just (StepQaReview, TaskStatusQa, AgentRoleQa)
escalationPlanFor StepQaReview = Just (StepFixIteration, TaskStatusImplementing, AgentRoleImplementer)
escalationPlanFor _ = Nothing

storeDesignArtifact :: ConnectionPool -> TaskId -> AgentResult -> IO ()
storeDesignArtifact pool taskId AgentResult { arSummary } =
  runSqlPool
    (storeArtifact taskId
      ( ArtifactDesign
      , "design-summary"
      , Just $ object ["summary" .= arSummary]
      )
    )
    pool

data CommandResult = CommandResult
  { crExitCode :: ExitCode
  , crStdout :: Text
  , crStderr :: Text
  }

commandSucceeded :: CommandResult -> Bool
commandSucceeded CommandResult { crExitCode = ExitSuccess } = True
commandSucceeded _ = False

commandResultValue :: CommandResult -> Value
commandResultValue CommandResult { crExitCode, crStdout, crStderr } =
  object
    [ "exitCode" .= show crExitCode
    , "stdout" .= crStdout
    , "stderr" .= crStderr
    ]

summarizeFailure :: CommandResult -> Text
summarizeFailure CommandResult { crStdout, crStderr } =
  let primary = T.strip crStderr
      fallback = T.strip crStdout
      picked = if T.null primary then fallback else primary
   in T.take 240 picked

runGit :: FilePath -> [String] -> IO CommandResult
runGit workingDir args = do
  (code, out, err) <- readCreateProcessWithExitCode (proc "git" args) { cwd = Just workingDir } ""
  pure CommandResult
    { crExitCode = code
    , crStdout = T.pack out
    , crStderr = T.pack err
    }

data CommitLogs = CommitLogs
  { clStatus :: CommandResult
  , clAdd :: CommandResult
  , clCommit :: Maybe CommandResult
  , clPush :: CommandResult
  }

commitLogsValue :: CommitLogs -> Value
commitLogsValue CommitLogs { clStatus, clAdd, clCommit, clPush } =
  object
    [ "status" .= commandResultValue clStatus
    , "add" .= commandResultValue clAdd
    , "commit" .= maybe Null (commandResultValue) clCommit
    , "push" .= commandResultValue clPush
    ]

storeCommitArtifact :: ConnectionPool -> TaskId -> CommitLogs -> IO ()
storeCommitArtifact pool taskId logs =
  runSqlPool
    (storeArtifact taskId
      ( ArtifactCommitLog
      , "commit-log"
      , Just (commitLogsValue logs)
      )
    )
    pool

updateRunAndStatus :: App -> TaskId -> WorkflowStep -> TaskStatus -> Text -> IO ()
updateRunAndStatus App { appConnPool } taskId step status summary = do
  now <- getCurrentTime
  runSqlPool
    (do
      updateLatestRun taskId step (Just summary)
      update taskId
        [ TaskStatus =. status
        , TaskUpdatedAt =. now
        ]
    )
    appConnPool

ensureLatestRun :: TaskId -> SqlPersistT IO TaskRunId
ensureLatestRun taskId = do
  mRun <- selectFirst [TaskRunTaskId ==. taskId] [Desc TaskRunOrdinal]
  case mRun of
    Just (Entity runId _) -> pure runId
    Nothing -> do
      now <- liftIO getCurrentTime
      insert TaskRun
        { taskRunTaskId = taskId
        , taskRunOrdinal = 1
        , taskRunCurrentStep = StepIntake
        , taskRunPmSummary = Nothing
        , taskRunCreatedAt = now
        , taskRunUpdatedAt = now
        }

updateLatestRun :: TaskId -> WorkflowStep -> Maybe Text -> SqlPersistT IO ()
updateLatestRun taskId step summary = do
  mRun <- selectFirst [TaskRunTaskId ==. taskId] [Desc TaskRunOrdinal]
  now <- liftIO getCurrentTime
  case mRun of
    Just (Entity runId _) ->
      update runId
        [ TaskRunCurrentStep =. step
        , TaskRunPmSummary =. summary
        , TaskRunUpdatedAt =. now
        ]
    Nothing -> pure ()

nextOrdinal :: TaskId -> SqlPersistT IO Int
nextOrdinal taskId = do
  latest <- selectFirst [TaskRunTaskId ==. taskId] [Desc TaskRunOrdinal]
  pure $ maybe 1 ((+ 1) . taskRunOrdinal . entityVal) latest

touchHeartbeat :: App -> TaskId -> IO ()
touchHeartbeat App { appHeartbeatVar } taskId = do
  now <- getCurrentTime
  atomically $ modifyTVar' appHeartbeatVar (Map.insert taskId now)

logEvent :: App -> TaskId -> WorkflowStep -> Text -> Maybe Value -> IO ()
broadcastAgentLog :: App -> TaskId -> WorkflowStep -> AgentRole -> Text -> IO ()
broadcastAgentLog app@App { appStatusHub } taskId step role rawMessage = do
  now <- getCurrentTime
  touchHeartbeat app taskId
  let (streamLabel, line) = classifyLogLine rawMessage
      payload = Just $ object
        [ "kind" .= ("agent-log" :: Text)
        , "stream" .= streamLabel
        , "line" .= line
        , "role" .= role
        ]
  publishStatus appStatusHub StatusEnvelope
    { envelopeTaskId = taskId
    , envelopeEvent = StatusEventDTO
        { statusEventId = Nothing
        , statusEventStep = step
        , statusEventMessage = line
        , statusEventCreatedAt = now
        , statusEventPayload = payload
        }
    }

classifyLogLine :: Text -> (Text, Text)
classifyLogLine message
  | Just rest <- T.stripPrefix "[stdout] " message = ("stdout", rest)
  | Just rest <- T.stripPrefix "[stderr] " message = ("stderr", rest)
  | Just rest <- T.stripPrefix "[publish] " message = ("publish", rest)
  | Just rest <- T.stripPrefix "[error] " message = ("error", rest)
  | otherwise = ("info", message)

logEvent app@App { appConnPool, appStatusHub } taskId step message payload = do
  now <- getCurrentTime
  touchHeartbeat app taskId
  eventId <- runSqlPool
    (insert StatusEvent
      { statusEventTaskId = taskId
      , statusEventStep = step
      , statusEventMessage = message
      , statusEventPayload = payload
      , statusEventCreatedAt = now
      }
    )
    appConnPool
  let dto = StatusEventDTO
        { statusEventId = Just (fromSqlKey eventId)
        , statusEventStep = step
        , statusEventMessage = message
        , statusEventCreatedAt = now
        , statusEventPayload = payload
        }
  publishStatus appStatusHub StatusEnvelope
    { envelopeTaskId = taskId
    , envelopeEvent = dto
    }

showText :: Show a => a -> Text
showText = T.pack . show

taskKeyText :: TaskId -> Text
taskKeyText = showText . fromSqlKey

taskSummaryFromEntity :: Set.Set TaskId -> Map.Map TaskId UTCTime -> Entity Task -> TaskSummary
taskSummaryFromEntity pausedSet snoozedMap (Entity key task) =
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
    , taskSummaryIsPaused = Set.member key pausedSet
    , taskSummarySnoozeUntil = Map.lookup key snoozedMap
    }

buildAgentContext :: TaskId -> WorkflowStep -> Maybe WorktreeContext -> Maybe Text -> SqlPersistT IO Value
buildAgentContext taskId step mWorktree mInstructions = do
  mTask <- get taskId
  events <- selectList [StatusEventTaskId ==. taskId] [Desc StatusEventCreatedAt, LimitTo 12]
  artifacts <- selectList [ArtifactTaskId ==. taskId] [Desc ArtifactCreatedAt, LimitTo 8]
  design <- selectFirst [ArtifactTaskId ==. taskId, ArtifactKind ==. ArtifactDesign] [Asc ArtifactCreatedAt]
  criteria <- selectList [TaskAcceptanceCriterionTaskId ==. taskId] [Asc TaskAcceptanceCriterionOrdinal]
  let taskValue = maybe (object ["missing" .= True]) taskToValue mTask
      worktreeValue = maybe Null worktreeToValue mWorktree
      instructionsValue = maybe Null toJSON mInstructions
      compactEvents = map statusEventToCompact events
      compactArtifacts = map artifactToMeta artifacts
      designValue = maybe Null designArtifactValue design
      progressSummary = summarizeEvents compactEvents
  pure $ object
    [ "task" .= taskValue
    , "workflowStep" .= toJSON step
    , "designSummary" .= designValue
    , "progressSummary" .= progressSummary
    , "recentEvents" .= compactEvents
    , "recentArtifacts" .= compactArtifacts
    , "acceptanceCriteria" .= map criterionToMeta criteria
    , "worktree" .= worktreeValue
    , "additionalInstructions" .= instructionsValue
    ]

taskToValue :: Task -> Value
taskToValue Task { taskTitle, taskDescription, taskRepoRoot, taskBranch, taskFeatureBranch, taskTestCommandOverride, taskStatus, taskPreviewUrl, taskPreviewStatus, taskCreatedAt, taskUpdatedAt } =
  object
    [ "title" .= taskTitle
    , "description" .= taskDescription
    , "repoRoot" .= taskRepoRoot
    , "branch" .= taskBranch
    , "featureBranch" .= taskFeatureBranch
    , "testCommandOverride" .= taskTestCommandOverride
    , "status" .= taskStatus
    , "previewUrl" .= taskPreviewUrl
    , "previewStatus" .= taskPreviewStatus
    , "createdAt" .= taskCreatedAt
    , "updatedAt" .= taskUpdatedAt
    ]

statusEventToCompact :: Entity StatusEvent -> Value
statusEventToCompact (Entity _ StatusEvent { statusEventStep, statusEventMessage, statusEventCreatedAt }) =
  object
    [ "step" .= statusEventStep
    , "message" .= statusEventMessage
    , "createdAt" .= statusEventCreatedAt
    ]

artifactToMeta :: Entity Artifact -> Value
artifactToMeta (Entity _ Artifact { artifactKind, artifactLabel, artifactPath, artifactCreatedAt, artifactContent }) =
  object
    [ "kind" .= artifactKind
    , "label" .= artifactLabel
    , "path" .= artifactPath
    , "createdAt" .= artifactCreatedAt
    , "body" .= artifactBody artifactKind artifactContent
    ]
  where
    artifactBody ArtifactTestLog (Just body) = body
    artifactBody ArtifactTestLog Nothing = Null
    artifactBody _ _ = Null

designArtifactValue :: Entity Artifact -> Value
designArtifactValue (Entity _ Artifact { artifactContent, artifactLabel, artifactCreatedAt }) =
  case artifactContent of
    Just (Object obj) ->
      case KeyMap.lookup (Key.fromText "summary") obj of
        Just (String summaryTxt) ->
          object
            [ "summary" .= truncateText 4000 summaryTxt
            , "label" .= artifactLabel
            , "createdAt" .= artifactCreatedAt
            ]
        _ -> object
          [ "label" .= artifactLabel
          , "createdAt" .= artifactCreatedAt
          ]
    Just (String txt) -> String (truncateText 4000 txt)
    Just val -> val
    Nothing ->
      object
        [ "label" .= artifactLabel
        , "createdAt" .= artifactCreatedAt
        , "note" .= String "Design artifact stored without content"
        ]

summarizeEvents :: [Value] -> Value
summarizeEvents events =
  let summaryLines = take 8 $ fmap summarizeOne events
   in String (T.intercalate "\n" summaryLines)
  where
    summarizeOne (Object obj) =
      let stepTxt = extractText "step" obj
          msgTxt = extractText "message" obj
       in case (stepTxt, msgTxt) of
            (Nothing, Nothing) -> ""
            (Just s, Nothing) -> s
            (Nothing, Just m) -> m
            (Just s, Just m) -> s <> ": " <> m
    summarizeOne _ = ""

    extractText key obj =
      case KeyMap.lookup (Key.fromText key) obj of
        Just (String txt) -> Just txt
        _ -> Nothing

truncateText :: Int -> Text -> Text
truncateText limit txt
  | T.length txt <= limit = txt
  | otherwise = T.take limit txt <> "…"

worktreeToValue :: WorktreeContext -> Value
worktreeToValue WorktreeContext { wtRoot, wtRepoRoot, wtBranch, wtTaskSlug } =
  object
    [ "root" .= wtRoot
    , "repoRoot" .= wtRepoRoot
    , "branch" .= wtBranch
    , "slug" .= wtTaskSlug
    ]

criterionToMeta :: Entity TaskAcceptanceCriterion -> Value
criterionToMeta (Entity _ TaskAcceptanceCriterion { taskAcceptanceCriterionBody = body, taskAcceptanceCriterionIsMet = isMet, taskAcceptanceCriterionOrdinal = ord }) =
  object
    [ "body" .= body
    , "isMet" .= isMet
    , "ordinal" .= ord
    ]

resolveWorkingDir :: Maybe WorktreeContext -> Maybe Task -> IO (Maybe FilePath)
resolveWorkingDir (Just wt) _ = pure (Just (wtRoot wt))
resolveWorkingDir Nothing (Just task)
  | T.null (taskRepoRoot task) = pure Nothing
  | otherwise = Just <$> canonicalizePath (T.unpack (taskRepoRoot task))
resolveWorkingDir Nothing Nothing = pure Nothing

instructionsFor :: AgentRole -> WorkflowStep -> Maybe Text -> Maybe Text
instructionsFor AgentRoleProjectManager StepDesign _ = Just $ T.unlines
  [ "Create a detailed technical design addressing the task goals."
  , "Summarise requirements, architecture, implementation plan, testing strategy, and risks."
  , "Produce the design in Markdown so downstream agents can rely on it."
  ]
instructionsFor AgentRoleProjectManager StepPmReview _ = Just $ T.unlines
  [ "Review the implemented work."
  , "Summarise the diff, note outstanding risks, and confirm readiness for QA."
  ]
instructionsFor AgentRoleImplementer StepImplementation mCmd = Just $ T.unlines
  [ "Work in the provided feature branch and workspace."
  , case mCmd of
      Just cmd -> "Create or update end-to-end tests demonstrating the desired behaviour, then run: " <> cmd
      Nothing -> "Create or update end-to-end tests demonstrating the desired behaviour before implementing the code."
  , case mCmd of
      Just cmd -> "Ensure the configured test command passes: " <> cmd
      Nothing -> "Ensure the project's tests pass before finishing."
  , "Leave changes ready for review without pushing." ]
instructionsFor AgentRoleQa StepQaReview _ = Just $ T.unlines
  [ "Review the diff and tests for bugs or security issues."
  , "Call out regressions, missing tests, or vulnerabilities with guidance for fixes."
  ]
instructionsFor AgentRoleVerifier StepSpecVerification _ = Just $ T.unlines
  [ "You are the autonomous verifier."
  , "Use the provided evidence bundle (screenshots, diff summaries, and agent artifacts) to confirm work satisfies the specification."
  , "Walk through every acceptance criterion in the context JSON and state whether it passes, citing the supporting artifact for each."
  , "Reference artifact labels when citing proof. Fail the step if any acceptance criterion is unmet or evidence is missing."
  , "Only approve when evidence proves the feature works end-to-end."
  ]
instructionsFor _ _ _ = Nothing
