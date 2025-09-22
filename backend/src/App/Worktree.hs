{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module App.Worktree
  ( WorktreeContext(..)
  , prepareWorktree
  , locateWorktree
  , cleanupWorktree
  , collectDiff
  , WorktreeResult(..)
  , runTestsInWorktree
  ) where

import App.Models (TaskId)
import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sql (fromSqlKey)
import System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , doesDirectoryExist
  , removePathForcibly
  )
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)

-- | Metadata about the worktree allocated for a task.
data WorktreeContext = WorktreeContext
  { wtRoot :: FilePath
  , wtRepoRoot :: FilePath
  , wtBranch :: Text
  , wtTaskSlug :: String
  }
  deriving (Show, Eq)

prepareWorktree :: FilePath -> Text -> TaskId -> IO WorktreeContext
prepareWorktree repoRoot baseBranch taskId = do
  ctx@WorktreeContext { wtRepoRoot, wtRoot, wtBranch } <- locateWorktree repoRoot taskId
  createDirectoryIfMissing True (takeDirectory wtRoot)
  gitExists <- doesDirectoryExist (wtRepoRoot </> ".git")
  when gitExists $ do
    _ <- try @SomeException (readCreateProcessWithExitCode (proc "git" ["-C", wtRepoRoot, "worktree", "remove", "--force", wtRoot]) "")
    pure ()
  existingDir <- doesDirectoryExist wtRoot
  when existingDir $ do
    _ <- try @SomeException (removePathForcibly wtRoot)
    pure ()
  when gitExists $ do
    _ <- try @SomeException (readCreateProcessWithExitCode (proc "git" ["-C", wtRepoRoot, "branch", "-D", T.unpack wtBranch]) "")
    pure ()
  when gitExists $ do
    createDirectoryIfMissing True (takeDirectory wtRoot)
    let cmd = proc "git"
          [ "-C", wtRepoRoot
          , "worktree"
          , "add"
          , "--force"
          , "-b"
          , T.unpack wtBranch
          , wtRoot
          , T.unpack baseBranch
          ]
    _ <- try @SomeException (readCreateProcessWithExitCode cmd "")
    pure ()
  unless gitExists $ createDirectoryIfMissing True wtRoot
  pure ctx

locateWorktree :: FilePath -> TaskId -> IO WorktreeContext
locateWorktree repoRoot taskId = do
  absoluteRepoRoot <- canonicalizePath repoRoot
  let slug = worktreeSlug taskId
      parentDir = takeDirectory absoluteRepoRoot
      worktreeBase = parentDir </> "worktrees"
      worktreePath = worktreeBase </> slug
      featureBranch = T.pack $ "task/" <> show (fromSqlKey taskId)
  pure WorktreeContext
    { wtRoot = worktreePath
    , wtRepoRoot = absoluteRepoRoot
    , wtBranch = featureBranch
    , wtTaskSlug = slug
    }

worktreeSlug :: TaskId -> String
worktreeSlug taskId = "task-" <> show (fromSqlKey taskId)

cleanupWorktree :: WorktreeContext -> IO ()
cleanupWorktree WorktreeContext { wtRoot, wtRepoRoot } = do
  gitExists <- doesDirectoryExist (wtRepoRoot </> ".git")
  when gitExists $ do
    let cmd = proc "git" ["-C", wtRepoRoot, "worktree", "remove", "--force", wtRoot]
    _ <- try @SomeException (readCreateProcessWithExitCode cmd "")
    pure ()
  _ <- try @SomeException (removePathForcibly wtRoot)
  pure ()

collectDiff :: WorktreeContext -> IO Text
collectDiff WorktreeContext { wtRoot } = do
  let cmd = proc "git" ["-C", wtRoot, "diff"]
  result <- try @SomeException (readCreateProcessWithExitCode cmd "")
  case result of
    Left _ -> pure "(git diff unavailable)"
    Right (exitCode, out, err) ->
      pure $ case exitCode of
        ExitSuccess -> T.pack out
        _ -> T.pack err

data WorktreeResult = WorktreeResult
  { wrExitCode :: ExitCode
  , wrStdout :: Text
  , wrStderr :: Text
  }
  deriving (Show, Eq)

runTestsInWorktree :: WorktreeContext -> Text -> IO WorktreeResult
runTestsInWorktree WorktreeContext { wtRoot } commandText
  | T.strip commandText == "" = pure WorktreeResult
      { wrExitCode = ExitSuccess
      , wrStdout = "Tests skipped (no command configured)"
      , wrStderr = ""
      }
  | otherwise = do
      let processSpec = (proc "bash" ["-lc", T.unpack commandText])
            { cwd = Just wtRoot }
      (code, out, err) <- readCreateProcessWithExitCode processSpec ""
      pure WorktreeResult
        { wrExitCode = code
        , wrStdout = T.pack out
        , wrStderr = T.pack err
        }
