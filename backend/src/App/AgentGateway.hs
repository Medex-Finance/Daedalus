{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module App.AgentGateway
  ( AgentGateway(..)
  , AgentInvocation(..)
  , AgentResult(..)
  , defaultGateway
  ) where

import App.Direnv (wrapWithDirenv)
import App.Models (TaskId)
import App.Types
import Control.Concurrent.Async (async, wait)
import Control.Exception (IOException, catchJust, displayException, finally, try)
import Control.Monad (unless)
import Data.Foldable (for_)
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, findExecutable)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode(..))
import System.IO (BufferMode(LineBuffering), Handle, hClose, hIsEOF, hSetBuffering)
import System.IO.Error (isEOFError)
import System.Process
  ( CreateProcess(..)
  , StdStream(CreatePipe)
  , ProcessHandle
  , proc
  , withCreateProcess
  , waitForProcess
  )
import System.FilePath (isPathSeparator)

-- | Request passed to an agent invocation.
data AgentInvocation = AgentInvocation
  { aiTaskId :: TaskId
  , aiRole :: AgentRole
  , aiPromptKey :: Text
  , aiPromptContent :: Text
  , aiContext :: Value
  , aiWorkingDir :: Maybe FilePath
  , aiEnvironment :: [(String, String)]
  , aiPublish :: Text -> IO ()
  , aiOnStart :: ProcessHandle -> IO ()
  , aiOnComplete :: IO ()
  }

-- | Result returned by an agent execution.
data AgentResult = AgentResult
  { arStatus :: AgentSessionStatus
  , arSummary :: Text
  , arStdout :: Text
  , arStderr :: Text
  , arArtifacts :: [(ArtifactKind, Text, Maybe Value)]
  }

newtype AgentGateway = AgentGateway
  { runAgent :: AgentInvocation -> IO AgentResult
  }

codexExecutableEnv, codexDefaultExecutable :: String
codexExecutableEnv = "CODEX_CLI"
codexDefaultExecutable = "codex"

defaultGateway :: AgentGateway
defaultGateway = AgentGateway runCodex

runCodex :: AgentInvocation -> IO AgentResult
runCodex AgentInvocation {..} = do
  execPath <- fromMaybe codexDefaultExecutable <$> lookupEnv codexExecutableEnv
  let prompt = assemblePrompt aiPromptContent aiContext
      args = ["exec", "--full-auto", "--skip-git-repo-check", "-"]
  selection <- selectCommand execPath args aiPublish
  case selection of
    Left err -> pure $ failureResult err
    Right CommandSelection { csExecutable, csArgs, csEnvExtras } -> do
      envWithCustom <- mergeEnv (csEnvExtras ++ aiEnvironment)
      ((execCmd, execArgs, finalEnv), direnvNote) <- wrapWithDirenv aiWorkingDir csExecutable csArgs envWithCustom
      for_ direnvNote $ \msg -> aiPublish ("[stderr] " <> msg)
      let processSpec = (proc execCmd execArgs)
            { cwd = aiWorkingDir
            , env = Just finalEnv
            , std_out = CreatePipe
            , std_err = CreatePipe
            , std_in = CreatePipe
            }
      let runProcess = withCreateProcess processSpec $ \mIn mOut mErr processHandle -> do
            aiOnStart processHandle
            case mIn of
              Just stdinHandle -> do
                hSetBuffering stdinHandle LineBuffering
                TIO.hPutStrLn stdinHandle (T.pack prompt)
                hClose stdinHandle
              Nothing -> pure ()
            stdoutAsync <- traverse (async . pumpLines "[stdout] " aiPublish) mOut
            stderrAsync <- traverse (async . pumpLines "[stderr] " aiPublish) mErr
            exitCode <- waitForProcess processHandle
            stdoutTxt <- maybe (pure "") wait stdoutAsync
            stderrTxt <- maybe (pure "") wait stderrAsync
            pure (exitCode, stdoutTxt, stderrTxt)
      result <- (try @IOException runProcess) `finally` aiOnComplete
      case result of
        Left err -> pure $ failureResult (T.pack $ displayException err)
        Right (exitCode, stdoutTxt, stderrTxt) -> do
          let transcript = object
                [ "prompt" .= prompt
                , "stdout" .= stdoutTxt
                , "stderr" .= stderrTxt
                , "exitCode" .= show exitCode
                , "workingDir" .= aiWorkingDir
                ]
              summary = deriveSummary aiRole exitCode stdoutTxt stderrTxt
              status = if exitCode == ExitSuccess then AgentSessionSucceeded else AgentSessionErrored
          pure AgentResult
            { arStatus = status
            , arSummary = summary
            , arStdout = stdoutTxt
            , arStderr = stderrTxt
            , arArtifacts =
                [ (ArtifactAgentTranscript, "codex-transcript.json", Just transcript)
                ]
            }
  where
    failureResult msg = AgentResult
      { arStatus = AgentSessionErrored
      , arSummary = "Codex invocation failed: " <> msg
      , arStdout = ""
      , arStderr = msg
      , arArtifacts =
          [ ( ArtifactAgentTranscript
            , "codex-error.json"
            , Just $ object ["error" .= msg]
            )
          ]
      }

pumpLines :: Text -> (Text -> IO ()) -> Handle -> IO Text
pumpLines prefix publish handle = do
  hSetBuffering handle LineBuffering
  chunksRef <- newIORef []
  let loop = do
        eof <- hIsEOF handle
        unless eof $ do
          maybeLine <- catchJust (\e -> if isEOFError e then Just () else Nothing)
                                  (Just <$> TIO.hGetLine handle)
                                  (\_ -> pure Nothing)
          case maybeLine of
            Nothing -> pure ()
            Just line -> do
              publish (prefix <> line)
              modifyIORef' chunksRef (<> [line])
              loop
  loop
  lines' <- readIORef chunksRef
  pure (T.intercalate "\n" lines')



assemblePrompt :: Text -> Value -> String
assemblePrompt basePrompt ctxValue =
  T.unpack $ basePrompt <> "\n\n---\nContext JSON:\n" <> decodeJson ctxValue

decodeJson :: Value -> Text
decodeJson = TE.decodeUtf8 . BL.toStrict . encode

mergeEnv :: [(String, String)] -> IO [(String, String)]
mergeEnv custom = do
  base <- getEnvironment
  let customKeys = map fst custom
      baseFiltered = filter ((`notElem` customKeys) . fst) base
  pure (custom ++ baseFiltered)

deriveSummary :: AgentRole -> ExitCode -> Text -> Text -> Text
deriveSummary role exitCode stdoutTxt stderrTxt =
  T.intercalate "\n"
    [ rolePrefix role
    , "Exit code: " <> T.pack (show exitCode)
    , "Output snippet: " <> snippet stdoutTxt stderrTxt
    ]

rolePrefix :: AgentRole -> Text
rolePrefix AgentRoleProjectManager = "Project manager agent run"
rolePrefix AgentRoleImplementer = "Implementation agent run"
rolePrefix AgentRoleQa = "QA agent run"

snippet :: Text -> Text -> Text
snippet stdoutTxt stderrTxt
  | not (T.null trimmedStdout) = T.take 400 trimmedStdout
  | not (T.null trimmedStderr) = T.take 400 trimmedStderr
  | otherwise = "(no output)"
  where
    trimmedStdout = T.strip stdoutTxt
    trimmedStderr = T.strip stderrTxt

data CommandSelection = CommandSelection
  { csExecutable :: FilePath
  , csArgs :: [String]
  , csEnvExtras :: [(String, String)]
  }

selectCommand :: String -> [String] -> (Text -> IO ()) -> IO (Either Text CommandSelection)
selectCommand execPath args publish = do
  resolved <- resolveExecutable execPath
  case resolved of
    Just exe -> pure . Right $ CommandSelection exe args []
    Nothing
      | execPath /= codexDefaultExecutable ->
          pure . Left $ missingMessage execPath
      | otherwise -> do
          npxPath <- resolveExecutable "npx"
          case npxPath of
            Just npxExe -> do
              publish "[stderr] codex executable not found; falling back to npx @openai/codex"
              let fallbackArgs = ["--yes", "@openai/codex", "codex"] ++ args
                  extras = [("NO_UPDATE_NOTIFIER", "1")]
              pure . Right $ CommandSelection npxExe fallbackArgs extras
            Nothing -> pure . Left $ missingMessage execPath

missingMessage :: String -> Text
missingMessage candidate = T.intercalate " "
  [ "Codex CLI executable"
  , quote candidate
  , "not found. Install it with 'npm install -g @openai/codex' or set CODEX_CLI to the executable path."
  ]

quote :: String -> Text
quote x = "'" <> T.pack x <> "'"

resolveExecutable :: String -> IO (Maybe FilePath)
resolveExecutable candidate
  | any isPathSeparator candidate = do
      exists <- doesFileExist candidate
      pure $ if exists then Just candidate else Nothing
  | otherwise = findExecutable candidate
