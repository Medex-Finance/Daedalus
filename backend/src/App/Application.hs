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
import Control.Concurrent.STM (newTVarIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Sqlite (createSqlitePool)
import Network.Wai (Application)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
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
  staticDirEnv <- lookupEnv "STATIC_DIR"
  let candidateStaticDir = fromMaybe "../frontend/dist" staticDirEnv
  candidateExists <- doesDirectoryExist candidateStaticDir
  let fallbackStaticDir = "static"
      staticDir = if candidateExists then candidateStaticDir else fallbackStaticDir
      indexFile = staticDir </> "index.html"
  createDirectoryIfMissing True fallbackStaticDir
  staticSite <- static staticDir
  let appFoundation = App
        { appConnPool = pool
        , appManager = manager
        , appSettingsVar = settingsVar
        , appPromptCache = promptVar
        , appQueue = queue
        , appStatusHub = hub
        , appAgentRegistry = agentRegistry
        , appHeartbeatVar = heartbeatVar
        , appStatic = staticSite
        , appIndexFile = indexFile
        }
  startOrchestrator appFoundation
  pure appFoundation

mkYesodDispatch "App" resourcesApp

appWai :: App -> IO Application
appWai = toWaiAppPlain
