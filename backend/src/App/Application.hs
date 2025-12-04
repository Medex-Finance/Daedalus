{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module App.Application
  ( makeApp
  , appWai
  ) where

import App.Foundation
import App.API
import App.Migrations (runMigrations)
import App.Orchestrator (startOrchestrator)
import App.PromptStore (ensurePromptTemplates)
import App.Queue (newQueue)
import App.Settings (ensureSettings)
import App.StatusStream (newStatusHub)
import App.RepoProfiles (loadRepoProfiles)
import Control.Concurrent.STM (newTVarIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Sqlite (createSqlitePool)
import Network.Wai (Application)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Text.Read (readMaybe)
import Yesod (mkYesodDispatch, toWaiAppPlain)
import Yesod.Static (static)

makeApp :: IO App
makeApp = do
  dbPath <- lookupEnv "DATABASE_PATH"
  let sqlitePath = T.pack $ maybe "orchestrator.sqlite3" id dbPath
  pool <- runStdoutLoggingT $ createSqlitePool sqlitePath 10
  runSqlPool runMigrations pool
  settingsDto <- ensureSettings pool
  promptCache <- ensurePromptTemplates pool
  settingsVar <- newTVarIO settingsDto
  promptVar <- newTVarIO promptCache
  queue <- newQueue
  hub <- newStatusHub
  manager <- makeHttpManager
  agentRegistry <- newTVarIO Map.empty
  heartbeatVar <- newTVarIO Map.empty
  workerStatesVar <- newTVarIO Map.empty
  workerAssignmentsVar <- newTVarIO Map.empty
  pausedTasksVar <- newTVarIO Set.empty
  snoozedTasksVar <- newTVarIO Map.empty
  workerMetricsVar <- newTVarIO Map.empty
  retryHintsVar <- newTVarIO Map.empty
  retryCountersVar <- newTVarIO Map.empty
  staticDirEnv <- lookupEnv "STATIC_DIR"
  workerCountEnv <- lookupEnv "WORKER_COUNT"
  let workerCount = fromMaybe 3 (workerCountEnv >>= readMaybe)
  let candidateStaticDir = fromMaybe "../frontend/dist" staticDirEnv
  candidateExists <- doesDirectoryExist candidateStaticDir
  let fallbackStaticDir = "static"
      staticDir = if candidateExists then candidateStaticDir else fallbackStaticDir
      indexFile = staticDir </> "index.html"
  createDirectoryIfMissing True fallbackStaticDir
  staticSite <- static staticDir
  repoProfiles <- loadRepoProfiles
  let appFoundation = App
        { appConnPool = pool
        , appManager = manager
        , appSettingsVar = settingsVar
        , appPromptCache = promptVar
        , appQueue = queue
        , appStatusHub = hub
        , appAgentRegistry = agentRegistry
        , appHeartbeatVar = heartbeatVar
        , appWorkerStates = workerStatesVar
        , appWorkerAssignments = workerAssignmentsVar
        , appPausedTasks = pausedTasksVar
        , appSnoozedTasks = snoozedTasksVar
        , appWorkerMetrics = workerMetricsVar
        , appRetryHints = retryHintsVar
        , appRetryCounters = retryCountersVar
        , appStatic = staticSite
        , appIndexFile = indexFile
        , appWorkerCount = workerCount
        , appRepoProfiles = repoProfiles
        }
  startOrchestrator appFoundation
  pure appFoundation

mkYesodDispatch "App" resourcesApp

appWai :: App -> IO Application
appWai = toWaiAppPlain
