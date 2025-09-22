{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module App.Foundation
  ( App(..)
  , AgentRuntime(..)
  , resourcesApp
  , Route(..)
  , Handler
  , makeHttpManager
  , getStatic
  , getIndexFile
  , getFrontendIndexR
  ) where

import App.Models (TaskId)
import App.Queue (AppQueue)
import App.StatusStream (StatusHub)
import App.Types (AgentRole, AppSettingsDTO, PromptTemplateDTO, WorkflowStep)
import Control.Concurrent.STM (TVar)
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.Sql (ConnectionPool, SqlBackend, runSqlPool)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Char8 as BS
import Network.Wai (rawPathInfo)
import Network.HTTP.Types.Status (status500)
import System.Directory (doesFileExist)
import System.Process (ProcessHandle)
import Yesod
import Yesod.Static (Static)

mkYesodData "App" $(parseRoutesFile "config/routes")

data AgentRuntime = AgentRuntime
  { agentHandle :: ProcessHandle
  , agentRole :: AgentRole
  , agentStep :: WorkflowStep
  , agentStartedAt :: UTCTime
  }

-- | Application foundation shared across handlers.
data App = App
  { appConnPool :: ConnectionPool
  , appManager :: Manager
  , appSettingsVar :: TVar AppSettingsDTO
  , appPromptCache :: TVar (Map Text PromptTemplateDTO)
  , appQueue :: AppQueue
  , appStatusHub :: StatusHub
  , appAgentRegistry :: TVar (Map TaskId AgentRuntime)
  , appHeartbeatVar :: TVar (Map TaskId UTCTime)
  , appStatic :: Static
  , appIndexFile :: FilePath
  }

instance Yesod App where
  makeSessionBackend _ = pure Nothing
  yesodMiddleware = defaultYesodMiddleware
  errorHandler NotFound = do
    req <- waiRequest
    let path = rawPathInfo req
        isApi = BS.isPrefixOf "/api" path
        isStatic = BS.isPrefixOf "/static" path
        isAsset = BS.elem '.' path
    if isApi || isStatic || isAsset
      then defaultErrorHandler NotFound
      else serveFrontendIndexHandler
  errorHandler other = defaultErrorHandler other

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    pool <- appConnPool <$> getYesod
    runSqlPool action pool

makeHttpManager :: IO Manager
makeHttpManager = newManager tlsManagerSettings

getStatic :: App -> Static
getStatic = appStatic

getIndexFile :: App -> FilePath
getIndexFile = appIndexFile

serveFrontendIndexHandler :: Handler TypedContent
serveFrontendIndexHandler = do
  app <- getYesod
  let indexFile = getIndexFile app
  exists <- liftIO $ doesFileExist indexFile
  if exists
    then sendFile typeHtml indexFile
    else sendResponseStatus status500 ("Frontend bundle not found. Build the UI or set STATIC_DIR." :: Text)

getFrontendIndexR :: Handler TypedContent
getFrontendIndexR = serveFrontendIndexHandler
