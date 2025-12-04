{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module OrchestratorSpec (spec, withTestApp) where

import App.Foundation
import App.Models
import App.Orchestrator
import App.Preview (startPreview, teardownPreview)
import App.Queue (AppQueue(..), QueueMessage(..), newQueue)
import App.Types
  ( AgentRole(..)
  , ArtifactKind(..)
  , OrchestratorSnapshot(..)
  , PreviewStatus(..)
  , SettingsUpdateRequest(..)
  , TaskStatus(..)
  , WorkflowStep(..)
  )
import App.Settings (ensureSettings, updateSettings)
import App.StatusStream (newStatusHub)
import App.Worktree (WorktreeContext(..), locateWorktree)
import App.RepoProfiles (RepoProfile(..), RepoCommand(..), RepoProfiles(..), emptyRepoProfiles)
import Control.Concurrent.STM
    ( atomically
    , modifyTVar'
    , newTVarIO
    , readTVarIO
    , tryReadTQueue
    , writeTVar
    )
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, bracket_, try)
import Control.Monad (forM_, void)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Either (isRight)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Database.Persist (Entity (..), SelectOpt (Desc), (==.))
import qualified Database.Persist as Persist
import Database.Persist.Sqlite (withSqlitePool)
import qualified Database.Persist.Sql as Psql (runSqlPool)
import System.Directory (canonicalizePath, createDirectoryIfMissing, getCurrentDirectory)
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>), takeDirectory)
import System.Process
    ( ProcessHandle
    , callProcess
    , createProcess
    , readProcess
    , proc
    , terminateProcess
    , waitForProcess
    )
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Yesod.Static (static)

import App.Migrations (runMigrations)
spec :: Spec
spec = do
  it "seeds worker state on orchestrator startup" $
    withTestApp $ \app -> do
      startOrchestrator app
      threadDelay 10000
      states <- readTVarIO (appWorkerStates app)
      Map.size states `shouldBe` 3
      metrics <- readTVarIO (appWorkerMetrics app)
      Map.size metrics `shouldBe` 3
      snapshot <- orchestratorSnapshot app
      length (snapshotWorkers snapshot) `shouldBe` 3

  it "interrupts the active agent and requeues the current step when pausing" $ 
    withTestApp $ \app -> withRuntime StepImplementation AgentRoleImplementer $ \runtime -> do
      taskId <- createTaskWithRun app StepImplementation
      registerRuntime app taskId runtime

      result <- pauseTask app taskId
      returned <- case result of
        Nothing -> expectationFailure "Expected runtime metadata from pauseTask" >> pure runtime
        Just r -> pure r
      agentStep returned `shouldBe` StepImplementation
      agentRole returned `shouldBe` AgentRoleImplementer

      pausedSet <- readTVarIO (appPausedTasks app)
      pausedSet `shouldSatisfy` Set.member taskId

      registry <- readTVarIO (appAgentRegistry app)
      Map.null registry `shouldBe` True

      msg <- atomically $ tryReadTQueue (queueGlobal (appQueue app))
      msg `shouldBe` Just (QueueAdvance taskId StepImplementation)

  it "clears pause state, records guidance, and requeues work on force retry" $
    withTestApp $ \app -> withRuntime StepImplementation AgentRoleImplementer $ \runtime -> do
      taskId <- createTaskWithRun app StepImplementation
      registerRuntime app taskId runtime

      now <- getCurrentTime
      atomically $ modifyTVar' (appPausedTasks app) (Set.insert taskId)
      atomically $ modifyTVar' (appSnoozedTasks app) (Map.insert taskId now)

      let guidance = Just "Focus on fixing the failing tests"
      outcome <- forceRetry app taskId guidance
      outcome `shouldBe` Right StepImplementation

      pausedSet <- readTVarIO (appPausedTasks app)
      pausedSet `shouldSatisfy` (not . Set.member taskId)

      snoozedMap <- readTVarIO (appSnoozedTasks app)
      snoozedMap `shouldSatisfy` (not . Map.member taskId)

      registry <- readTVarIO (appAgentRegistry app)
      Map.null registry `shouldBe` True

      msg <- atomically $ tryReadTQueue (queueGlobal (appQueue app))
      msg `shouldBe` Just (QueueAdvance taskId StepImplementation)

      retryCounters <- readTVarIO (appRetryCounters app)
      retryCounters `shouldSatisfy` (not . Map.member taskId)

      events <- Psql.runSqlPool (Persist.selectList [StatusEventTaskId ==. taskId] [Desc StatusEventCreatedAt]) (appConnPool app)
      case events of
        (Entity _ StatusEvent { statusEventMessage = message, statusEventPayload = payload } : _) -> do
          message `shouldBe` "Manual force retry requested"
          assertPayload payload guidance
        [] -> expectationFailure "Expected status event to be recorded"

  it "consumes automation report output before Gemini" $ do
    backendDir <- canonicalizePath =<< getCurrentDirectory
    let projectRoot = takeDirectory backendDir
        sandboxDir = projectRoot </> "demo" </> "verifier-sandbox"
        fakeCodex = projectRoot </> "scripts" </> "fake-codex.sh"
    setEnv "VERIFIER_SANDBOX_ROOT" sandboxDir
    setEnv "VERIFIER_DISABLE_PLAYWRIGHT" "1"
    setEnv "CODEX_CLI" fakeCodex
    setEnv "GEMINI_FAKE_MODE" "1"
    setEnv "GEMINI_API_KEY" "test-key"
    withTestApp $ \app -> do
      taskId <- prepareVerifierTask app backendDir
      runSpecVerification app taskId
      let pool = appConnPool app
      artifacts <- Psql.runSqlPool (Persist.selectList [ArtifactTaskId ==. taskId] [Desc ArtifactCreatedAt]) pool
      let kinds = fmap (artifactKind . entityVal) artifacts
          labels = fmap (artifactLabel . entityVal) artifacts
      kinds `shouldSatisfy` elem ArtifactScreenshot
      kinds `shouldSatisfy` elem ArtifactVerificationEvidence
      kinds `shouldSatisfy` elem ArtifactVerifierReport
      labels `shouldSatisfy` elem "automation-verdict"
      labels `shouldSatisfy` elem "gemini-verdict"

  it "falls back to Gemini when automation report is missing" $ do
    backendDir <- canonicalizePath =<< getCurrentDirectory
    let projectRoot = takeDirectory backendDir
        sandboxDir = projectRoot </> "demo" </> "verifier-sandbox"
        fakeCodex = projectRoot </> "scripts" </> "fake-codex.sh"
        overrideCmd = "bash -lc 'rm -f automation-report.json && cp reference.png current.png'"
    setEnv "VERIFIER_SANDBOX_ROOT" sandboxDir
    setEnv "VERIFIER_DISABLE_PLAYWRIGHT" "1"
    setEnv "CODEX_CLI" fakeCodex
    setEnv "GEMINI_FAKE_MODE" "1"
    setEnv "GEMINI_API_KEY" "test-key"
    bracket_ (setEnv "VERIFIER_CAPTURE_COMMAND" overrideCmd) (unsetEnv "VERIFIER_CAPTURE_COMMAND") $
      withTestApp $ \app -> do
        taskId <- prepareVerifierTask app backendDir
        runSpecVerification app taskId
        let pool = appConnPool app
        artifacts <- Psql.runSqlPool (Persist.selectList [ArtifactTaskId ==. taskId] [Desc ArtifactCreatedAt]) pool
        let labels = fmap (artifactLabel . entityVal) artifacts
            kinds = fmap (artifactKind . entityVal) artifacts
        kinds `shouldSatisfy` elem ArtifactVerifierReport
        labels `shouldSatisfy` elem "gemini-verdict"

  it "runs an offline end-to-end pipeline through finalize" $ do
    backendDir <- canonicalizePath =<< getCurrentDirectory
    let projectRoot = takeDirectory backendDir
        sandboxDir = projectRoot </> "demo" </> "verifier-sandbox"
    withTempRepo $ \repoRoot tempDir -> do
      codexScript <- writeFixingCodex tempDir
      let restore = mapM_ unsetEnv ["VERIFIER_SANDBOX_ROOT", "VERIFIER_DISABLE_PLAYWRIGHT", "GEMINI_FAKE_MODE", "CODEX_CLI", "DEFAULT_REPO_ROOT"]
      bracket_ (do
        setEnv "VERIFIER_SANDBOX_ROOT" sandboxDir
        setEnv "VERIFIER_DISABLE_PLAYWRIGHT" "1"
        setEnv "GEMINI_FAKE_MODE" "1"
        setEnv "CODEX_CLI" codexScript
        setEnv "DEFAULT_REPO_ROOT" repoRoot
        ) restore $
        withTestApp $ \app -> do
          let pool = appConnPool app
              settingsReq = SettingsUpdateRequest
                { settingsRepoRoot = T.pack repoRoot
                , settingsBranch = "main"
                , settingsUpdateTestCommand = "bash -lc '[[ \"$(cat status.txt 2>/dev/null)\" == \"fixed\" ]]'"
                , settingsUpdatePreviewCommand = Just "python3 -m http.server $PORT"
                , settingsUpdateInactivityMinutes = 5
                }
          updatedSettings <- updateSettings pool settingsReq
          atomically $ writeTVar (appSettingsVar app) updatedSettings
          startOrchestrator app
          now <- getCurrentTime
          taskId <- Psql.runSqlPool
            (Persist.insert Task
              { taskTitle = "E2E Task"
              , taskDescription = "Full pipeline"
              , taskStatus = TaskStatusPending
              , taskRepoRoot = T.pack repoRoot
              , taskBranch = "main"
              , taskFeatureBranch = Nothing
              , taskTestCommandOverride = Nothing
              , taskPreviewUrl = Nothing
              , taskPreviewStatus = PreviewOffline
              , taskCreatedAt = now
              , taskUpdatedAt = now
              })
            pool
          Psql.runSqlPool
            (Persist.insert_ TaskAcceptanceCriterion
              { taskAcceptanceCriterionTaskId = taskId
              , taskAcceptanceCriterionOrdinal = 1
              , taskAcceptanceCriterionBody = "UI matches reference"
              , taskAcceptanceCriterionIsMet = False
              , taskAcceptanceCriterionCreatedAt = now
              , taskAcceptanceCriterionUpdatedAt = now
              })
            pool
          enqueueTask app taskId
          awaitStatus app taskId TaskStatusReviewing 120
          awaitPreviewReady app taskId 120
          previewOutcome <- startPreview app taskId
          previewOutcome `shouldSatisfy` isRight
          enqueueTaskStep app taskId StepFinalize
          awaitStatus app taskId TaskStatusCompleted 120
          artifacts <- Psql.runSqlPool (Persist.selectList [ArtifactTaskId ==. taskId] []) pool
          let artifactStrings = fmap (BL8.unpack . encodePretty . maybe Null id . artifactContent . entityVal) artifacts
          artifactStrings `shouldSatisfy` any (isInfixOf "status.txt")
          artifactStrings `shouldSatisfy` any (isInfixOf "fixed")
          artifacts `shouldSatisfy` any (\(Entity _ Artifact { artifactLabel }) -> artifactLabel == "commit-log")
          callProcess "git" ["-C", repoRoot, "fetch"]
          let branchName = "task/" <> show (fromSqlKey taskId)
          statusText <- readProcess "git" ["-C", repoRoot, "show", "origin/" <> branchName <> ":status.txt"] ""
          statusText `shouldBe` "fixed\n"
          teardownPreview pool taskId

  it "runs a profile-driven verifier and captures both automation and gemini verdicts" $ do
    backendDir <- canonicalizePath =<< getCurrentDirectory
    let projectRoot = takeDirectory backendDir
        sandboxDir = projectRoot </> "demo" </> "verifier-sandbox"
    withTempRepo $ \repoRoot tempDir -> do
      codexScript <- writeFixingCodex tempDir
      let profile = RepoProfile
            { rpName = "temp-profile"
            , rpRoot = repoRoot
            , rpSandboxOverride = Just sandboxDir
            , rpCaptureCommand = Nothing
            , rpEnv = Map.fromList [("VERIFIER_AUTOMATION_URL", "file://" <> sandboxDir </> "demo.html")]
            , rpSetup = []
            }
          profiles = RepoProfiles [profile]
          restore = mapM_ unsetEnv ["VERIFIER_SANDBOX_ROOT", "VERIFIER_DISABLE_PLAYWRIGHT", "GEMINI_FAKE_MODE", "CODEX_CLI", "DEFAULT_REPO_ROOT"]
      bracket_ (do
        setEnv "VERIFIER_DISABLE_PLAYWRIGHT" "1"
        setEnv "GEMINI_FAKE_MODE" "1"
        setEnv "CODEX_CLI" codexScript
        setEnv "DEFAULT_REPO_ROOT" repoRoot
        ) restore $
        withTestAppWithProfiles profiles $ \app -> do
          let pool = appConnPool app
              settingsReq = SettingsUpdateRequest
                { settingsRepoRoot = T.pack repoRoot
                , settingsBranch = "main"
                , settingsUpdateTestCommand = "echo ok"
                , settingsUpdatePreviewCommand = Just "python3 -m http.server $PORT"
                , settingsUpdateInactivityMinutes = 5
                }
          updatedSettings <- updateSettings pool settingsReq
          atomically $ writeTVar (appSettingsVar app) updatedSettings
          startOrchestrator app
          now <- getCurrentTime
          taskId <- Psql.runSqlPool
            (Persist.insert Task
              { taskTitle = "Profile verifier task"
              , taskDescription = "Ensure profile sandbox is used"
              , taskStatus = TaskStatusImplementing
              , taskRepoRoot = T.pack repoRoot
              , taskBranch = "main"
              , taskFeatureBranch = Nothing
              , taskTestCommandOverride = Nothing
              , taskPreviewUrl = Nothing
              , taskPreviewStatus = PreviewOffline
              , taskCreatedAt = now
              , taskUpdatedAt = now
              })
            pool
          Psql.runSqlPool
            (Persist.insert_ TaskAcceptanceCriterion
              { taskAcceptanceCriterionTaskId = taskId
              , taskAcceptanceCriterionOrdinal = 1
              , taskAcceptanceCriterionBody = "UI matches reference"
              , taskAcceptanceCriterionIsMet = False
              , taskAcceptanceCriterionCreatedAt = now
              , taskAcceptanceCriterionUpdatedAt = now
              })
            pool
          runSpecVerification app taskId
          artifacts <- Psql.runSqlPool (Persist.selectList [ArtifactTaskId ==. taskId] [Desc ArtifactCreatedAt]) pool
          let labels = fmap (artifactLabel . entityVal) artifacts
          labels `shouldSatisfy` elem "automation-verdict"
          labels `shouldSatisfy` elem "gemini-verdict"

withTestApp :: (App -> IO a) -> IO a
withTestApp = withTestAppWithProfiles emptyRepoProfiles

withTestAppWithProfiles :: RepoProfiles -> (App -> IO a) -> IO a
withTestAppWithProfiles profiles action = runNoLoggingT $ withSqlitePool ":memory:" 1 $ \pool -> liftIO $ do
  Psql.runSqlPool runMigrations pool
  settings <- ensureSettings pool
  settingsVar <- newTVarIO settings
  promptVar <- newTVarIO Map.empty
  queue <- newQueue
  hub <- newStatusHub
  manager <- makeHttpManager
  agentRegistry <- newTVarIO Map.empty
  heartbeatVar <- newTVarIO Map.empty
  workerStates <- newTVarIO Map.empty
  assignments <- newTVarIO Map.empty
  paused <- newTVarIO Set.empty
  snoozed <- newTVarIO Map.empty
  workerMetricsVar <- newTVarIO Map.empty
  retryHints <- newTVarIO Map.empty
  retryCounters <- newTVarIO Map.empty
  staticSite <- static "static"
  let app = App
        { appConnPool = pool
        , appManager = manager
        , appSettingsVar = settingsVar
        , appPromptCache = promptVar
        , appQueue = queue
        , appStatusHub = hub
        , appAgentRegistry = agentRegistry
        , appHeartbeatVar = heartbeatVar
        , appWorkerStates = workerStates
        , appWorkerAssignments = assignments
        , appPausedTasks = paused
        , appSnoozedTasks = snoozed
        , appWorkerMetrics = workerMetricsVar
        , appRetryHints = retryHints
        , appRetryCounters = retryCounters
        , appStatic = staticSite
        , appIndexFile = ""
        , appWorkerCount = 3
        , appRepoProfiles = profiles
        }
  action app

createTaskWithRun :: App -> WorkflowStep -> IO TaskId
createTaskWithRun app step = do
  now <- getCurrentTime
  let pool = appConnPool app
      taskEntity = Task
        { taskTitle = "Integration task"
        , taskDescription = ""
        , taskStatus = TaskStatusImplementing
        , taskRepoRoot = "."
        , taskBranch = "main"
        , taskFeatureBranch = Nothing
        , taskTestCommandOverride = Nothing
        , taskPreviewUrl = Nothing
        , taskPreviewStatus = PreviewOffline
        , taskCreatedAt = now
        , taskUpdatedAt = now
        }
  taskId <- Psql.runSqlPool (Persist.insert taskEntity) pool
  let runEntity = TaskRun
        { taskRunTaskId = taskId
        , taskRunOrdinal = 1
        , taskRunCurrentStep = step
        , taskRunPmSummary = Nothing
        , taskRunCreatedAt = now
        , taskRunUpdatedAt = now
        }
  Psql.runSqlPool (Persist.insert_ runEntity) pool
  pure taskId

prepareVerifierTask :: App -> FilePath -> IO TaskId
prepareVerifierTask app repoRoot = do
  now <- getCurrentTime
  let pool = appConnPool app
  taskId <- Psql.runSqlPool
    (Persist.insert Task
      { taskTitle = "Verifier E2E Task"
      , taskDescription = "Ensure capture pipeline works"
      , taskStatus = TaskStatusImplementing
      , taskRepoRoot = T.pack repoRoot
      , taskBranch = "main"
      , taskFeatureBranch = Nothing
      , taskTestCommandOverride = Nothing
      , taskPreviewUrl = Nothing
      , taskPreviewStatus = PreviewOffline
      , taskCreatedAt = now
      , taskUpdatedAt = now
      })
    pool
  Psql.runSqlPool
    (Persist.insert_ TaskRun
      { taskRunTaskId = taskId
      , taskRunOrdinal = 1
      , taskRunCurrentStep = StepSpecVerification
      , taskRunPmSummary = Nothing
      , taskRunCreatedAt = now
      , taskRunUpdatedAt = now
      })
    pool
  Psql.runSqlPool
    (Persist.insert_ TaskAcceptanceCriterion
      { taskAcceptanceCriterionTaskId = taskId
      , taskAcceptanceCriterionOrdinal = 1
      , taskAcceptanceCriterionBody = "UI screenshot matches reference"
      , taskAcceptanceCriterionIsMet = False
      , taskAcceptanceCriterionCreatedAt = now
      , taskAcceptanceCriterionUpdatedAt = now
      })
    pool
  worktree <- locateWorktree repoRoot taskId
  createDirectoryIfMissing True (wtRoot worktree)
  pure taskId

registerRuntime :: App -> TaskId -> AgentRuntime -> IO ()
registerRuntime App { appAgentRegistry, appHeartbeatVar } taskId runtime = do
  atomically $ modifyTVar' appAgentRegistry (Map.insert taskId runtime)
  atomically $ modifyTVar' appHeartbeatVar (Map.insert taskId (agentStartedAt runtime))

withRuntime :: WorkflowStep -> AgentRole -> (AgentRuntime -> IO a) -> IO a
withRuntime step role action = bracket acquire release (action . fst)
  where
    acquire :: IO (AgentRuntime, ProcessHandle)
    acquire = do
      (_, _, _, ph) <- createProcess (proc "sleep" ["60"])
      startedAt <- getCurrentTime
      let runtime = AgentRuntime
            { agentHandle = ph
            , agentRole = role
            , agentStep = step
            , agentStartedAt = startedAt
            }
      pure (runtime, ph)
    release :: (AgentRuntime, ProcessHandle) -> IO ()
    release (_, ph) = void $ try @SomeException $ do
      terminateProcess ph
      _ <- waitForProcess ph
      pure ()

assertPayload :: Maybe Value -> Maybe Text -> Expectation
assertPayload Nothing _ = expectationFailure "Expected status event payload"
assertPayload (Just (Object obj)) guidance = do
  lookupText "origin" obj `shouldBe` Just "human"
  forM_ guidance $ \textInstruction ->
    lookupText "instructions" obj `shouldBe` Just textInstruction
  lookupText "interruptedStep" obj `shouldBe` Just "StepImplementation"
  lookupText "interruptedRole" obj `shouldBe` Just "AgentRoleImplementer"
assertPayload _ _ = expectationFailure "Expected payload object"

lookupText :: Text -> KeyMap.KeyMap Value -> Maybe Text
lookupText key obj =
  case KeyMap.lookup (Key.fromText key) obj of
    Just (String txt) -> Just txt
    _ -> Nothing

withTempRepo :: (FilePath -> FilePath -> IO a) -> IO a
withTempRepo action =
  withSystemTempDirectory "orchestrator-e2e" $ \dir -> do
    let repoRoot = dir </> "repo"
        origin = dir </> "origin.git"
    callProcess "git" ["init", "--bare", origin]
    callProcess "git" ["init", repoRoot]
    callProcess "git" ["-C", repoRoot, "config", "user.email", "test@example.com"]
    callProcess "git" ["-C", repoRoot, "config", "user.name", "tester"]
    callProcess "git" ["-C", repoRoot, "checkout", "-b", "main"]
    writeFile (repoRoot </> "README.md") "# temp repo\n"
    writeFile (repoRoot </> "status.txt") "broken\n"
    callProcess "git" ["-C", repoRoot, "add", "."]
    callProcess "git" ["-C", repoRoot, "commit", "-m", "init"]
    callProcess "git" ["-C", repoRoot, "remote", "add", "origin", origin]
    callProcess "git" ["-C", repoRoot, "push", "-u", "origin", "main"]
    action repoRoot dir

writeFixingCodex :: FilePath -> IO FilePath
writeFixingCodex dir = do
  let script = dir </> "fake-codex-fix.sh"
  writeFile script $ unlines
    [ "#!/usr/bin/env bash"
    , "set -euo pipefail"
    , "if [[ \"$#\" -gt 0 && \"$1\" == \"exec\" ]]; then shift; fi"
    , "# swallow stdin"
    , "cat >/dev/null || true"
    , "echo \"fixed\" > status.txt"
    , "echo \"[fake-codex] applied fix\" >&2"
    , "echo \"[fake-codex] run completed successfully\""
    ]
  callProcess "chmod" ["+x", script]
  pure script

awaitStatus :: App -> TaskId -> TaskStatus -> Int -> IO ()
awaitStatus app taskId desired maxSeconds = loop 0
  where
    loop n = do
      mTask <- Psql.runSqlPool (Persist.get taskId) (appConnPool app)
      case mTask of
        Just task | taskStatus task == desired -> pure ()
        _ ->
          if n >= maxSeconds
            then expectationFailure ("Timed out waiting for status " <> show desired)
            else threadDelay 1000000 >> loop (n + 1)

awaitPreviewReady :: App -> TaskId -> Int -> IO ()
awaitPreviewReady app taskId maxSeconds = loop 0
  where
    loop n = do
      mTask <- Psql.runSqlPool (Persist.get taskId) (appConnPool app)
      case mTask of
        Just task
          | taskPreviewUrl task /= Nothing -> pure ()
        _ ->
          if n >= maxSeconds
            then expectationFailure "Timed out waiting for preview URL"
            else threadDelay 1000000 >> loop (n + 1)
