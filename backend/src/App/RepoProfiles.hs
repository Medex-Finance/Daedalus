{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module App.RepoProfiles
  ( RepoProfiles(..)
  , RepoProfile(..)
  , RepoCommand(..)
  , loadRepoProfiles
  , lookupRepoProfile
  , runRepoProfileSetup
  , emptyRepoProfiles
  ) where
import Control.Exception (SomeException, displayException, try)
import Control.Monad (foldM)
import Data.Aeson (FromJSON(..), (.:), (.:?), withObject)
import qualified Data.Aeson as Aeson
import Data.List (find, isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Directory (canonicalizePath, doesFileExist, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath (pathSeparator, (</>), takeDirectory, isAbsolute)
import System.Process (CreateProcess(..), readCreateProcessWithExitCode, shell)
import System.Timeout (timeout)
import System.Exit (ExitCode(..))
import qualified Data.ByteString.Lazy as BL

data RepoCommand = RepoCommand
  { rcName :: Text
  , rcCommand :: Text
  , rcWorkingDir :: Maybe FilePath
  , rcTimeoutSeconds :: Maybe Int
  }
  deriving (Show, Eq, Generic)

data RepoProfile = RepoProfile
  { rpName :: Text
  , rpRoot :: FilePath
  , rpSandboxOverride :: Maybe FilePath
  , rpCaptureCommand :: Maybe Text
  , rpEnv :: Map String String
  , rpSetup :: [RepoCommand]
  }
  deriving (Show, Eq, Generic)

newtype RepoProfiles = RepoProfiles
  { unRepoProfiles :: [RepoProfile]
  }
  deriving (Show, Eq)

emptyRepoProfiles :: RepoProfiles
emptyRepoProfiles = RepoProfiles []

data RepoCommandSpec = RepoCommandSpec
  { rcsName :: Maybe Text
  , rcsCommand :: Text
  , rcsWorkingDir :: Maybe FilePath
  , rcsTimeoutSeconds :: Maybe Int
  }
  deriving (Show, Generic)

instance FromJSON RepoCommandSpec where
  parseJSON = withObject "RepoCommand" $ \obj ->
    RepoCommandSpec
      <$> obj .:? "name"
      <*> obj .: "command"
      <*> obj .:? "workingDir"
      <*> obj .:? "timeoutSeconds"

data RepoProfileSpec = RepoProfileSpec
  { specMatch :: FilePath
  , specName :: Maybe Text
  , specSandbox :: Maybe FilePath
  , specCaptureCommand :: Maybe Text
  , specEnv :: Maybe (Map String String)
  , specSetup :: Maybe [RepoCommandSpec]
  }
  deriving (Show, Generic)

instance FromJSON RepoProfileSpec where
  parseJSON = withObject "RepoProfile" $ \obj ->
    RepoProfileSpec
      <$> obj .: "match"
      <*> obj .:? "name"
      <*> obj .:? "sandbox"
      <*> obj .:? "captureCommand"
      <*> obj .:? "env"
      <*> obj .:? "setup"

loadRepoProfiles :: IO RepoProfiles
loadRepoProfiles = do
  pathEnv <- lookupEnv "REPO_PROFILES_PATH"
  let rawPath = fromMaybe "config/repo-profiles.json" pathEnv
  path <- resolveProfilePath rawPath
  exists <- doesFileExist path
  if not exists
    then pure emptyRepoProfiles
    else do
      bytes <- BL.readFile path
      case Aeson.eitherDecode bytes of
        Left err -> do
          putStrLn $ "[repo-profiles] Failed to parse " <> path <> ": " <> err
          pure emptyRepoProfiles
        Right specs -> do
          profiles <- mapM buildProfile specs
          pure $ RepoProfiles profiles
  where
    buildProfile RepoProfileSpec { specMatch, specName, specSandbox, specCaptureCommand, specEnv, specSetup } = do
      canonical <- canonicalizePath specMatch
      let commands = map toCommand (fromMaybe [] specSetup)
      pure RepoProfile
        { rpName = fromMaybe (T.pack specMatch) specName
        , rpRoot = canonical
        , rpSandboxOverride = specSandbox
        , rpCaptureCommand = specCaptureCommand
        , rpEnv = fromMaybe Map.empty specEnv
        , rpSetup = commands
        }
    toCommand RepoCommandSpec { rcsName, rcsCommand, rcsWorkingDir, rcsTimeoutSeconds } =
      RepoCommand
        { rcName = fromMaybe "repo-command" rcsName
        , rcCommand = rcsCommand
        , rcWorkingDir = rcsWorkingDir
        , rcTimeoutSeconds = rcsTimeoutSeconds
        }

resolveProfilePath :: FilePath -> IO FilePath
resolveProfilePath path
  | isAbsolute path = pure path
  | otherwise = do
      cwd <- getCurrentDirectory
      let candidate = cwd </> path
          parent = takeDirectory cwd
          parentCandidate = parent </> path
      exists <- doesFileExist candidate
      if exists
        then pure candidate
        else do
          parentExists <- doesFileExist parentCandidate
          pure $ if parentExists then parentCandidate else candidate

lookupRepoProfile :: RepoProfiles -> FilePath -> IO (Maybe RepoProfile)
lookupRepoProfile (RepoProfiles profiles) repoRoot = do
  canonical <- canonicalizePath repoRoot
  let decorated = decorate canonical
  pure $
    find
      (\RepoProfile { rpRoot } -> matches (decorate rpRoot) decorated)
      profiles
  where
    decorate path =
      let base = if null path then [pathSeparator] else path
       in if last base == pathSeparator
            then base
            else base <> [pathSeparator]
    matches base target =
      base `isPrefixOf` target

runRepoProfileSetup :: RepoProfile -> IO (Either Text ())
runRepoProfileSetup profile@RepoProfile { rpSetup } = do
  foldM step (Right ()) rpSetup
  where
    step (Left err) _ = pure (Left err)
    step (Right _) command = runCommand profile command

runCommand :: RepoProfile -> RepoCommand -> IO (Either Text ())
runCommand RepoProfile { rpRoot } RepoCommand {.. } = do
  let dir = fromMaybe rpRoot rcWorkingDir
      procSpec = (shell (T.unpack rcCommand)) { cwd = Just dir }
      timeoutMicros = fmap (* 1000000) rcTimeoutSeconds
  execResult <-
    case timeoutMicros of
      Nothing -> tryRun procSpec
      Just micros -> do
        timed <- timeout micros (tryRun procSpec)
        pure $ fromMaybe (Left "command timed out") timed
  pure $
    case execResult of
      Left err -> Left (rcName <> ": " <> err)
      Right () -> Right ()
  where
    tryRun cp = do
      result <- try @SomeException (readCreateProcessWithExitCode cp "")
      case result of
        Left err -> pure . Left $ T.pack (displayException err)
        Right (code, out, errText) ->
          if code == ExitSuccess
            then pure (Right ())
            else
              let summary = T.intercalate " " (filter (not . T.null) [T.pack out, T.pack errText])
               in pure (Left summary)
