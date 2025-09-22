{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module App.Orchestrator
  ( startOrchestrator
  , enqueueTask
  , orchestratorSnapshot
  , logEvent
  , cancelTask
  ) where

import App.AgentGateway
import App.Foundation (AgentRuntime(..), App(..))
import App.Logging (logError, logInfo)
import App.Models
import App.Preview
import App.Queue
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
import Control.Concurrent.STM (atomically, modifyTVar', readTVar, readTVarIO)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value(..), object, (.=), toJSON)
import qualified Data.Aeson as Aeson
import Data.Foldable (for_)
import Data.List (find)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import qualified Data.Map.Strict as Map
import Control.Exception (SomeException, try)
import Database.Persist.Sql
  ( Entity(..)
  , SqlPersistT
  , SelectOpt(..)
  , entityKey
  , entityVal
  , fromSqlKey
  , get
  , insert
  , insert_
  , runSqlPool
  , selectFirst
  , selectList
  , update
  , (=.)
  , (==.)
  )
import System.Directory (canonicalizePath)
import System.Exit (ExitCode(..))
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode, terminateProcess)

startOrchestrator :: App -> IO ()
startOrchestrator app = do
  _ <- async (workerLoop app)
  _ <- async (inactivityMonitor app)
  logInfo "Orchestrator worker started"
  pure ()

workerLoop :: App -> IO ()
workerLoop app@App { appQueue } = forever $ do
  msg <- dequeue appQueue
  case msg of
    QueueKickoff taskId -> processKickoff app taskId
    QueueAdvance taskId step -> processStep app taskId step

enqueueTask :: App -> TaskId -> IO ()
enqueueTask App { appQueue } taskId = enqueue appQueue (QueueKickoff taskId)

orchestratorSnapshot :: App -> IO OrchestratorSnapshot
orchestratorSnapshot App { appConnPool, appQueue } = do
  entities <- runSqlPool (selectList [] [Desc TaskUpdatedAt]) appConnPool
  let summaries = fmap taskSummaryFromEntity entities
  depth <- queueSize appQueue
  pure OrchestratorSnapshot
    { snapshotActiveTasks = summaries
    , snapshotQueueDepth = depth
    }

processKickoff :: App -> TaskId -> IO ()
processKickoff app@App { appConnPool, appQueue } taskId = do
  outcome <- runSqlPool action appConnPool
  case outcome of
    Nothing -> logError $ "Kickoff requested for missing task " <> T.pack (show taskId)
    Just _ -> do
      logEvent app taskId StepIntake "Task scheduled" Nothing
      enqueue appQueue (QueueAdvance taskId StepDesign)
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
          StepPmReview -> runPmReview app taskId
          StepQaReview -> runQaReview app taskId
          StepCommit -> runCommit app taskId
          StepPreview -> runPreviewStep app taskId
          StepFinalize -> finalizeTask app taskId
          StepFixIteration -> runFixIteration app taskId
          StepIntake -> pure ()

runDesign :: App -> TaskId -> IO ()
runDesign app@App { appConnPool, appQueue } taskId = do
  result <- runAgentSession app taskId StepDesign AgentRoleProjectManager "pm_design" Nothing Nothing
  halted <- shouldAbortTask app taskId
  if halted
    then logInfo $ "Design step post-processing skipped for halted task " <> taskKeyText taskId
    else if agentSucceeded result
      then do
        updateRunAndStatus app taskId StepDesign TaskStatusDesigning (arSummary result)
        logEvent app taskId StepDesign "Technical design produced" (Just $ object ["summary" .= arSummary result])
        storeDesignArtifact appConnPool taskId result
        enqueue appQueue (QueueAdvance taskId StepImplementation)
      else markBlocked app taskId StepDesign (arSummary result) Nothing

runImplementation :: App -> TaskId -> IO ()
runImplementation app@App { appConnPool, appQueue, appSettingsVar } taskId = do
  mTask <- runSqlPool (get taskId) appConnPool
  case mTask of
    Nothing -> logError $ "Implementation requested for missing task " <> taskKeyText taskId
    Just task -> do
      let repoRoot = T.unpack (taskRepoRoot task)
          branch = taskBranch task
      worktree <- prepareWorktree repoRoot branch taskId
      runSqlPool (update taskId [TaskFeatureBranch =. Just (wtBranch worktree)]) appConnPool
      settings <- readTVarIO appSettingsVar
      let testCommand = settingsTestCommand settings
      result <- runAgentSession app taskId StepImplementation AgentRoleImplementer "implementer" (Just worktree) (Just testCommand)
      halted <- shouldAbortTask app taskId
      if halted
        then logInfo $ "Implementation step post-processing skipped for halted task " <> taskKeyText taskId
        else if agentSucceeded result
          then do
            updateRunAndStatus app taskId StepImplementation TaskStatusImplementing (arSummary result)
            logEvent app taskId StepImplementation "Implementation agent run completed" (Just $ object ["summary" .= arSummary result])
            tests <- runTestsInWorktree worktree testCommand
            storeTestArtifact appConnPool taskId testCommand tests
            diffText <- collectDiff worktree
            storeDiffArtifact appConnPool taskId diffText
            case wrExitCode tests of
              ExitSuccess -> enqueue appQueue (QueueAdvance taskId StepPmReview)
              code ->
                let payload = object
                      [ "exitCode" .= show code
                      , "command" .= testCommand
                      ]
                    summary = "Test command failed; see worktree-tests artifact"
                 in markBlocked app taskId StepImplementation summary (Just payload)
          else markBlocked app taskId StepImplementation (arSummary result) Nothing

runPmReview :: App -> TaskId -> IO ()
runPmReview app@App { appQueue } taskId = do
  result <- runAgentSession app taskId StepPmReview AgentRoleProjectManager "pm_design" Nothing Nothing
  halted <- shouldAbortTask app taskId
  if halted
    then logInfo $ "PM review post-processing skipped for halted task " <> taskKeyText taskId
    else if agentSucceeded result
      then do
        updateRunAndStatus app taskId StepPmReview TaskStatusReviewing (arSummary result)
        logEvent app taskId StepPmReview "Project manager review complete" (Just $ object ["summary" .= arSummary result])
        enqueue appQueue (QueueAdvance taskId StepQaReview)
      else markBlocked app taskId StepPmReview (arSummary result) Nothing

runQaReview :: App -> TaskId -> IO ()
runQaReview app@App { appConnPool, appQueue } taskId = do
  result <- runAgentSession app taskId StepQaReview AgentRoleQa "qa_reviewer" Nothing Nothing
  halted <- shouldAbortTask app taskId
  if halted
    then logInfo $ "QA review post-processing skipped for halted task " <> taskKeyText taskId
    else if agentSucceeded result
      then case parseQaVerdict result of
        Left err -> markBlocked app taskId StepQaReview ("QA verdict parsing failed: " <> err) Nothing
        Right verdict@QaVerdict { qvApproved = True } -> do
          updateRunAndStatus app taskId StepQaReview TaskStatusQa (arSummary result)
          logEvent app taskId StepQaReview "QA review confirms readiness" (Just $ object ["summary" .= arSummary result])
          storeQaArtifact appConnPool taskId verdict result
          enqueue appQueue (QueueAdvance taskId StepCommit)
        Right verdict@QaVerdict { qvApproved = False, qvIssues = issues } -> do
          storeQaArtifact appConnPool taskId verdict result
          let payload = Just $ object ["issues" .= maybe Null id issues]
          markBlocked app taskId StepQaReview "QA reported issues that require fixes" payload
          enqueue appQueue (QueueAdvance taskId StepFixIteration)
      else markBlocked app taskId StepQaReview (arSummary result) Nothing

runFixIteration :: App -> TaskId -> IO ()
runFixIteration app@App { appQueue } taskId = do
  updateRunAndStatus app taskId StepFixIteration TaskStatusImplementing "Fix iteration scheduled"
  logEvent app taskId StepFixIteration "Re-running implementation to address QA feedback" Nothing
  enqueue appQueue (QueueAdvance taskId StepImplementation)

cancelTask :: App -> TaskId -> IO ()
cancelTask app@App { appConnPool } taskId = do
  mTask <- runSqlPool (get taskId) appConnPool
  case mTask of
    Nothing -> logError $ "Cancel requested for missing task " <> taskKeyText taskId
    Just _ -> do
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
runCommit app@App { appConnPool, appQueue } taskId = do
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
          enqueue appQueue (QueueAdvance taskId StepPreview)

runPreviewStep :: App -> TaskId -> IO ()
runPreviewStep app@App { appQueue } taskId = do
  outcome <- startPreview app taskId
  case outcome of
    Left err -> markBlocked app taskId StepPreview ("Preview launch failed: " <> err) (Just $ object ["error" .= err])
    Right url -> do
      let summary = "Preview registered: " <> url <> " (pending manual startup)"
      updateRunAndStatus app taskId StepPreview TaskStatusReviewing summary
      logEvent app taskId StepPreview "Preview environment registered; awaiting launch" (Just $ object ["url" .= url])
      enqueue appQueue (QueueAdvance taskId StepFinalize)

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

runAgentSession :: App -> TaskId -> WorkflowStep -> AgentRole -> Text -> Maybe WorktreeContext -> Maybe Text -> IO AgentResult
runAgentSession app@App { appConnPool, appAgentRegistry, appHeartbeatVar } taskId step role promptKey mWorktree mTestCommand = do
  template <- runSqlPool (selectFirst [PromptTemplateKey ==. promptKey] []) appConnPool
  mTask <- runSqlPool (get taskId) appConnPool
  let instructions = instructionsFor role step mTestCommand
      basePrompt = maybe "" (promptTemplateContent . entityVal) template
      extraInstructions = maybe "" (\txt -> "\n\nAdditional Instructions:\n" <> txt) instructions
      augmentedPrompt = basePrompt <> extraInstructions
  context <- runSqlPool (buildAgentContext taskId step mWorktree instructions) appConnPool
  workingDir <- resolveWorkingDir mWorktree mTask
  touchHeartbeat app taskId
  let publishStream = broadcastAgentLog app taskId step
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
  result <- runAgent gateway invocation
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

storeArtifact :: TaskId -> (ArtifactKind, Text, Maybe Value) -> SqlPersistT IO ()
storeArtifact taskId (kind, label, payload) = do
  now <- liftIO getCurrentTime
  insert_ Artifact
    { artifactTaskId = taskId
    , artifactKind = kind
    , artifactLabel = label
    , artifactContent = payload
    , artifactPath = Nothing
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

handleInactivityTimeout :: App -> TaskId -> AgentRuntime -> NominalDiffTime -> Int -> IO ()
handleInactivityTimeout app@App { appQueue } taskId runtime age minutes = do
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
      logEvent app taskId (agentStep activeRuntime) "Agent heartbeat timed out; restarting step" payload
      halted <- shouldAbortTask app taskId
      when (not halted) $
        enqueue appQueue (QueueAdvance taskId (agentStep activeRuntime))

markBlocked :: App -> TaskId -> WorkflowStep -> Text -> Maybe Value -> IO ()
markBlocked app taskId step summary payload = do
  updateRunAndStatus app taskId step TaskStatusBlocked summary
  logEvent app taskId step summary payload

data QaVerdict = QaVerdict
  { qvApproved :: Bool
  , qvIssues :: Maybe Value
  }

parseQaVerdict :: AgentResult -> Either Text QaVerdict
parseQaVerdict AgentResult { arStdout } =
  case verdictLine of
    Nothing -> Left "QA_VERDICT marker missing"
    Just line ->
      case T.stripPrefix verdictPrefix line of
        Nothing -> Left "QA_VERDICT malformed"
        Just verdictTxt ->
          let normalized = T.toUpper (T.strip verdictTxt)
           in if normalized == "PASS"
                then Right QaVerdict { qvApproved = True, qvIssues = Nothing }
                else if normalized `elem` ["FAIL", "REJECTED", "CHANGES_REQUESTED"]
                  then do
                    issuesValue <- traverse decodeIssues issuesLine
                    Right QaVerdict { qvApproved = False, qvIssues = issuesValue }
                  else Left ("Unexpected QA verdict: " <> normalized)
  where
    verdictPrefix = "QA_VERDICT:"
    issuesPrefix = "QA_ISSUES_JSON:"
    linesDesc = reverse (T.lines arStdout)
    verdictLine = find (T.isPrefixOf verdictPrefix) linesDesc
    issuesLine = find (T.isPrefixOf issuesPrefix) linesDesc
    decodeIssues txt =
      let raw = T.strip $ T.drop (T.length issuesPrefix) txt
       in case Aeson.eitherDecodeStrict' (TE.encodeUtf8 raw) of
            Left err -> Left ("Failed to decode QA_ISSUES_JSON: " <> T.pack err)
            Right value -> Right value
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

storeQaArtifact :: ConnectionPool -> TaskId -> QaVerdict -> AgentResult -> IO ()
storeQaArtifact pool taskId QaVerdict { qvApproved, qvIssues } AgentResult { arSummary } =
  runSqlPool
    (storeArtifact taskId
      ( ArtifactTestLog
      , "qa-verdict"
      , Just $ object
          [ "verdict" .= if qvApproved then "pass" :: Text else "fail"
          , "issues" .= maybe Null id qvIssues
          , "summary" .= arSummary
          ]
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
broadcastAgentLog :: App -> TaskId -> WorkflowStep -> Text -> IO ()
broadcastAgentLog app@App { appStatusHub } taskId step message = do
  now <- getCurrentTime
  touchHeartbeat app taskId
  publishStatus appStatusHub StatusEnvelope
    { envelopeTaskId = taskId
    , envelopeEvent = StatusEventDTO
        { statusEventStep = step
        , statusEventMessage = message
        , statusEventCreatedAt = now
        , statusEventPayload = Nothing
        }
    }

logEvent app@App { appConnPool, appStatusHub } taskId step message payload = do
  now <- getCurrentTime
  touchHeartbeat app taskId
  runSqlPool
    (insert_ StatusEvent
      { statusEventTaskId = taskId
      , statusEventStep = step
      , statusEventMessage = message
      , statusEventPayload = payload
      , statusEventCreatedAt = now
      }
    )
    appConnPool
  let dto = StatusEventDTO
        { statusEventStep = step
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

buildAgentContext :: TaskId -> WorkflowStep -> Maybe WorktreeContext -> Maybe Text -> SqlPersistT IO Value
buildAgentContext taskId step mWorktree mInstructions = do
  mTask <- get taskId
  events <- selectList [StatusEventTaskId ==. taskId] [Desc StatusEventCreatedAt, LimitTo 10]
  artifacts <- selectList [ArtifactTaskId ==. taskId] [Desc ArtifactCreatedAt, LimitTo 10]
  let taskValue = maybe (object ["missing" .= True]) taskToValue mTask
      worktreeValue = maybe Null worktreeToValue mWorktree
      instructionsValue = maybe Null toJSON mInstructions
  pure $ object
    [ "task" .= taskValue
    , "workflowStep" .= toJSON step
    , "recentEvents" .= map statusEventToValue events
    , "recentArtifacts" .= map artifactToValue artifacts
    , "worktree" .= worktreeValue
    , "additionalInstructions" .= instructionsValue
    ]

taskToValue :: Task -> Value
taskToValue Task { taskTitle, taskDescription, taskRepoRoot, taskBranch, taskFeatureBranch, taskStatus, taskPreviewUrl, taskPreviewStatus, taskCreatedAt, taskUpdatedAt } =
  object
    [ "title" .= taskTitle
    , "description" .= taskDescription
    , "repoRoot" .= taskRepoRoot
    , "branch" .= taskBranch
    , "featureBranch" .= taskFeatureBranch
    , "status" .= taskStatus
    , "previewUrl" .= taskPreviewUrl
    , "previewStatus" .= taskPreviewStatus
    , "createdAt" .= taskCreatedAt
    , "updatedAt" .= taskUpdatedAt
    ]

statusEventToValue :: Entity StatusEvent -> Value
statusEventToValue (Entity _ StatusEvent { statusEventStep, statusEventMessage, statusEventCreatedAt, statusEventPayload }) =
  object
    [ "step" .= statusEventStep
    , "message" .= statusEventMessage
    , "createdAt" .= statusEventCreatedAt
    , "payload" .= statusEventPayload
    ]

artifactToValue :: Entity Artifact -> Value
artifactToValue (Entity _ Artifact { artifactKind, artifactLabel, artifactContent, artifactPath, artifactCreatedAt }) =
  object
    [ "kind" .= artifactKind
    , "label" .= artifactLabel
    , "content" .= artifactContent
    , "path" .= artifactPath
    , "createdAt" .= artifactCreatedAt
    ]

worktreeToValue :: WorktreeContext -> Value
worktreeToValue WorktreeContext { wtRoot, wtRepoRoot, wtBranch, wtTaskSlug } =
  object
    [ "root" .= wtRoot
    , "repoRoot" .= wtRepoRoot
    , "branch" .= wtBranch
    , "slug" .= wtTaskSlug
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
instructionsFor _ _ _ = Nothing
