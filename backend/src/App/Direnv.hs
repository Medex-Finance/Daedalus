{-# LANGUAGE OverloadedStrings #-}
module App.Direnv
  ( wrapWithDirenv
  ) where

import Control.Exception (SomeException, displayException, try)
import qualified Data.Text as T
import Data.Text (Text)
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)
import Data.List (deleteBy)
import Data.Function (on)

type CommandSpec = (FilePath, [String], [(String, String)])

-- | Wrap a command with "direnv exec" so it runs inside the nix shell managed by direnv.
-- Returns the possibly updated executable/args/environment along with a warning if direnv
-- could not be used and the command will run without the shell.
wrapWithDirenv :: Maybe FilePath -> FilePath -> [String] -> [(String, String)] -> IO (CommandSpec, Maybe Text)
wrapWithDirenv Nothing cmd args envVars = pure ((cmd, args, envVars), Nothing)
wrapWithDirenv (Just workingDir) cmd args envVars = do
  mDirenv <- findExecutable "direnv"
  case mDirenv of
    Nothing ->
      pure ((cmd, args, envVars), Just "direnv executable not found; running command without nix shell environment")
    Just direnvExe -> do
      let envWithLog = setDirenvLog envVars
      allowResult <- runDirenvAllow direnvExe workingDir envWithLog
      case allowResult of
        Left warn -> pure ((cmd, args, envVars), Just warn)
        Right () ->
          let wrappedArgs = ["exec", workingDir, cmd] ++ args
           in pure ((direnvExe, wrappedArgs, envWithLog), Nothing)

setDirenvLog :: [(String, String)] -> [(String, String)]
setDirenvLog envVars = ("DIRENV_LOG_FORMAT", "") : filtered
  where
    filtered = deleteBy ((==) `on` fst) ("DIRENV_LOG_FORMAT", "") envVars

runDirenvAllow :: FilePath -> FilePath -> [(String, String)] -> IO (Either Text ())
runDirenvAllow direnvExe workingDir envVars = do
  let allowProc = (proc direnvExe ["allow"]) { cwd = Just workingDir, env = Just envVars }
  result <- try (readCreateProcessWithExitCode allowProc "")
    :: IO (Either SomeException (ExitCode, String, String))
  case result of
    Left err ->
      pure . Left $ "direnv allow failed: " <> T.pack (displayException err)
    Right (ExitSuccess, _, _) -> pure (Right ())
    Right (ExitFailure code, _, stderrOut) ->
      pure . Left $ "direnv allow exited with " <> T.pack (show code) <> ": " <> T.strip (T.pack stderrOut)
