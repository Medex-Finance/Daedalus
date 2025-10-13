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

import App.Foundation (App(..))
import App.Logging (logError)
import App.Models (TaskId)
import App.ProcessDispatcher (ProcessOutput(..), runCommandInDir)
import App.Types (AgentRole(..), WorkflowStep(..))
import Control.Exception (SomeException, try)
import Control.Monad (unless, void, when)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sql (fromSqlKey)
import System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , removePathForcibly
  )
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.Process (proc, readCreateProcessWithExitCode)

-- | Metadata about the worktree allocated for a task.
data WorktreeContext = WorktreeContext
  { wtRoot :: FilePath
  , wtRepoRoot :: FilePath
  , wtBranch :: Text
  , wtTaskSlug :: String
  }
  deriving (Show, Eq)

prepareWorktree :: App -> FilePath -> Text -> TaskId -> Bool -> IO WorktreeContext
prepareWorktree app repoRoot baseBranch taskId forceReset = do
  ctx@WorktreeContext { wtRepoRoot, wtRoot, wtBranch } <- locateWorktree repoRoot taskId
  createDirectoryIfMissing True (takeDirectory wtRoot)
  gitExists <- doesDirectoryExist (wtRepoRoot </> ".git")
  when gitExists $ do
    when forceReset $ resetExistingWorktree wtRepoRoot wtRoot wtBranch
    ensureBranchExists wtRepoRoot wtBranch baseBranch
    ensureWorktreePresent wtRepoRoot wtRoot wtBranch
  unless gitExists $ createDirectoryIfMissing True wtRoot
  void $ runSetupInstall app taskId ctx
  pure ctx
  where
    ensureBranchExists :: FilePath -> Text -> Text -> IO ()
    ensureBranchExists repo branch base = do
      let branchName = T.unpack branch
          baseName = T.unpack base
          ref = "refs/heads/" <> branchName
      (code, _, _) <- readCreateProcessWithExitCode (proc "git" ["-C", repo, "show-ref", "--verify", "--quiet", ref]) ""
      case code of
        ExitSuccess -> pure ()
        _ ->
          unless (null baseName) $
            void $ try @SomeException (readCreateProcessWithExitCode (proc "git" ["-C", repo, "branch", branchName, baseName]) "")

    ensureWorktreePresent :: FilePath -> FilePath -> Text -> IO ()
    ensureWorktreePresent repo root branch = do
      exists <- doesDirectoryExist root
      indicator <- doesFileExist (root </> ".git")
      if exists && indicator
        then pure ()
        else do
          let branchName = T.unpack branch
              cmd = proc "git"
                [ "-C", repo
                , "worktree"
                , "add"
                , root
                , branchName
                ]
          _ <- try @SomeException (readCreateProcessWithExitCode cmd "")
          pure ()

    resetExistingWorktree :: FilePath -> FilePath -> Text -> IO ()
    resetExistingWorktree repo root branch = do
      _ <- try @SomeException (readCreateProcessWithExitCode (proc "git" ["-C", repo, "worktree", "remove", "--force", root]) "")
      _ <- try @SomeException (readCreateProcessWithExitCode (proc "git" ["-C", repo, "branch", "-D", T.unpack branch]) "")
      exists <- doesDirectoryExist root
      when exists $ do
        _ <- try @SomeException (removePathForcibly root)
        pure ()

    runSetupInstall :: App -> TaskId -> WorktreeContext -> IO ()
    runSetupInstall appCtx taskIdCtx ctx@WorktreeContext { wtRoot } = do
      let manifestPath = wtRoot </> "package.json"
      manifestExists <- doesFileExist manifestPath
      when manifestExists $ do
        let commandText = "pnpm install"
        ProcessOutput { poExitCode = code, poStdout = out, poStderr = err } <-
          runCommandInDir appCtx taskIdCtx StepImplementation AgentRoleImplementer "pnpm-install" wtRoot commandText
        unless (code == ExitSuccess) $
          logSetupFailure ctx out err

    logSetupFailure WorktreeContext { wtTaskSlug } out err =
      logError $ "pnpm install failed for worktree " <> T.pack wtTaskSlug <> ": " <> summarizeFailure out err

summarizeFailure :: Text -> Text -> Text
summarizeFailure out err =
  let trimmedErr = T.strip err
      trimmedOut = T.strip out
   in if T.null trimmedErr then T.take 240 trimmedOut else T.take 240 trimmedErr

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

runTestsInWorktree :: App -> TaskId -> WorkflowStep -> AgentRole -> WorktreeContext -> Text -> IO WorktreeResult
runTestsInWorktree _ _ _ _ _ commandText | T.strip commandText == "" =
  pure WorktreeResult
    { wrExitCode = ExitSuccess
    , wrStdout = "Tests skipped (no command configured)"
    , wrStderr = ""
    }
runTestsInWorktree app taskId step role WorktreeContext { wtRoot } commandText = do
  ProcessOutput { poExitCode = code, poStdout = out, poStderr = err } <-
    runCommandInDir app taskId step role "tests" wtRoot commandText
  pure WorktreeResult
    { wrExitCode = code
    , wrStdout = out
    , wrStderr = err
    }
