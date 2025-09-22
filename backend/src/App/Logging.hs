{-# LANGUAGE OverloadedStrings #-}
module App.Logging
  ( logInfo
  , logWarn
  , logError
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

logWith :: T.Text -> T.Text -> IO ()
logWith level msg = do
  now <- getCurrentTime
  let stamp = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now
  T.putStrLn $ T.intercalate " " ["[" <> stamp <> "]", level <> ":", msg]

logInfo :: T.Text -> IO ()
logInfo = logWith "INFO"

logWarn :: T.Text -> IO ()
logWarn = logWith "WARN"

logError :: T.Text -> IO ()
logError = logWith "ERROR"
