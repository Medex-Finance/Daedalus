{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import App.Application (makeApp)
import App.Foundation (App(..))
import App.Models
import App.Types hiding (taskStatus)
import Control.Monad (forM_)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  App { appConnPool } <- makeApp
  now <- getCurrentTime
  runSqlPool (insertSamples now args) appConnPool
  putStrLn "Seed data inserted."

insertSamples :: UTCTime -> [String] -> SqlPersistT IO ()
insertSamples now args = do
  let baseTitle = if null args then "Sample Feature" else T.pack (unwords args)
      mkTask n = Task
        { taskTitle = baseTitle <> " #" <> T.pack (show n)
        , taskDescription = "Demo task for orchestration pipeline"
        , taskStatus = TaskStatusPending
        , taskRepoRoot = "."
        , taskBranch = "main"
        , taskFeatureBranch = Nothing
        , taskPreviewUrl = Nothing
        , taskPreviewStatus = PreviewOffline
        , taskCreatedAt = now
        , taskUpdatedAt = now
        }
  forM_ ([1 .. 3] :: [Int]) $ \n -> insert (mkTask n)
