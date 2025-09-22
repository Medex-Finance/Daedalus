module Main where

import App.Application (appWai, makeApp)
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  portEnv <- lookupEnv "PORT"
  foundation <- makeApp
  application <- appWai foundation
  let port = maybe 4000 read portEnv
  putStrLn $ "Starting backend on port " <> show port
  run port application
