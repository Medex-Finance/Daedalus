{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module App.Settings
  ( loadSettings
  , updateSettings
  , ensureSettings
  ) where

import App.Models
import App.Types
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Time.Clock (getCurrentTime)
import Data.Text (Text)
import qualified Data.Text as T

defaultTestCommand :: Text
defaultTestCommand = "./scripts/run-tests.sh"

defaultInactivityMinutes :: Int
defaultInactivityMinutes = 5

ensureSettings :: ConnectionPool -> IO AppSettingsDTO
ensureSettings pool = runSqlPool ensure pool
  where
    ensure = do
      existing <- selectFirst [] []
      case existing of
        Just (Entity sid s) -> do
          when (T.null $ appSettingsTestCommand s) $
            update sid [AppSettingsTestCommand =. defaultTestCommand]
          when (appSettingsInactivityTimeoutMinutes s <= 0) $
            update sid [AppSettingsInactivityTimeoutMinutes =. defaultInactivityMinutes]
          let normalized =
                s
                  { appSettingsTestCommand =
                      if T.null (appSettingsTestCommand s)
                        then defaultTestCommand
                        else appSettingsTestCommand s
                  , appSettingsInactivityTimeoutMinutes =
                      if appSettingsInactivityTimeoutMinutes s <= 0
                        then defaultInactivityMinutes
                        else appSettingsInactivityTimeoutMinutes s
                  }
          pure (toDTO normalized)
        Nothing -> do
          now <- liftIO getCurrentTime
          let repo = "."
          _ <- insert AppSettings
                { appSettingsDefaultRepoRoot = repo
                , appSettingsDefaultBranch = "main"
                , appSettingsTestCommand = defaultTestCommand
                , appSettingsPreviewCommand = Nothing
                , appSettingsInactivityTimeoutMinutes = defaultInactivityMinutes
                , appSettingsUpdatedAt = now
                }
          pure AppSettingsDTO
                { settingsDefaultRepoRoot = repo
                , settingsDefaultBranch = "main"
                , settingsTestCommand = defaultTestCommand
                , settingsPreviewCommand = Nothing
                , settingsInactivityMinutes = defaultInactivityMinutes
                , settingsUpdatedAt = now
                }

loadSettings :: ConnectionPool -> IO AppSettingsDTO
loadSettings = ensureSettings

updateSettings :: ConnectionPool -> SettingsUpdateRequest -> IO AppSettingsDTO
updateSettings pool SettingsUpdateRequest { settingsRepoRoot, settingsBranch, settingsUpdateTestCommand, settingsUpdatePreviewCommand, settingsUpdateInactivityMinutes } =
  runSqlPool action pool
  where
    normalizedTimeout = max 1 settingsUpdateInactivityMinutes
    action = do
      existing <- selectFirst [] []
      now <- liftIO getCurrentTime
      case existing of
        Just (Entity sid _) -> do
          update sid
            [ AppSettingsDefaultRepoRoot =. settingsRepoRoot
            , AppSettingsDefaultBranch =. settingsBranch
            , AppSettingsTestCommand =. settingsUpdateTestCommand
            , AppSettingsPreviewCommand =. settingsUpdatePreviewCommand
            , AppSettingsInactivityTimeoutMinutes =. normalizedTimeout
            , AppSettingsUpdatedAt =. now
            ]
        Nothing -> do
          _ <- insert AppSettings
            { appSettingsDefaultRepoRoot = settingsRepoRoot
            , appSettingsDefaultBranch = settingsBranch
            , appSettingsTestCommand = settingsUpdateTestCommand
            , appSettingsPreviewCommand = settingsUpdatePreviewCommand
            , appSettingsInactivityTimeoutMinutes = normalizedTimeout
            , appSettingsUpdatedAt = now
            }
          pure ()
      pure AppSettingsDTO
        { settingsDefaultRepoRoot = settingsRepoRoot
        , settingsDefaultBranch = settingsBranch
        , settingsTestCommand = settingsUpdateTestCommand
        , settingsPreviewCommand = settingsUpdatePreviewCommand
        , settingsInactivityMinutes = normalizedTimeout
        , settingsUpdatedAt = now
        }

toDTO :: AppSettings -> AppSettingsDTO
toDTO AppSettings { appSettingsDefaultRepoRoot, appSettingsDefaultBranch, appSettingsTestCommand, appSettingsPreviewCommand, appSettingsInactivityTimeoutMinutes, appSettingsUpdatedAt } =
  AppSettingsDTO
    { settingsDefaultRepoRoot = appSettingsDefaultRepoRoot
    , settingsDefaultBranch = appSettingsDefaultBranch
    , settingsTestCommand = if T.null appSettingsTestCommand then defaultTestCommand else appSettingsTestCommand
    , settingsPreviewCommand = appSettingsPreviewCommand
    , settingsInactivityMinutes =
        if appSettingsInactivityTimeoutMinutes <= 0 then defaultInactivityMinutes else appSettingsInactivityTimeoutMinutes
    , settingsUpdatedAt = appSettingsUpdatedAt
    }
