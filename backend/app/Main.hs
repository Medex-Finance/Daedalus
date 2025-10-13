module Main where

import App.Application (appWai, makeApp)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Network.Wai.Handler.Warp (HostPreference, defaultSettings, runSettings, setHost, setPort)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  portEnv <- lookupEnv "PORT"
  hostEnv <- lookupEnv "HOST"
  foundation <- makeApp
  application <- appWai foundation
  let port = maybe 4000 read portEnv
      hostPref :: HostPreference
      hostPref = maybe (fromString "*4") fromString hostEnv
      settings = setPort port . setHost hostPref $ defaultSettings
      hostDescription = fromMaybe "0.0.0.0" hostEnv
  putStrLn $ "Starting backend on " <> hostDescription <> ":" <> show port
  runSettings settings application
