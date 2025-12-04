{-# LANGUAGE OverloadedStrings #-}
module EvidenceSpec (spec) where

import App.Evidence (EvidenceCapture(..), EvidenceOptions(..), defaultEvidenceOptions, collectDemoEvidence)
import App.Worktree (WorktreeContext(..))
import Data.Aeson (Value)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (setEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "Evidence capture pipeline" $ do
  it "produces deterministic capture artifacts" $ do
    setEnv "VERIFIER_DISABLE_PLAYWRIGHT" "1"
    backendDir <- getCurrentDirectory
    let projectRoot = takeDirectory backendDir
    setEnv "VERIFIER_SANDBOX_ROOT" (projectRoot </> "demo" </> "verifier-sandbox")
    let repoRoot = backendDir
    withSystemTempDirectory "verifier-wt" $ \tmpDir -> do
      let worktree = WorktreeContext
            { wtRoot = tmpDir
            , wtRepoRoot = repoRoot
            , wtBranch = "test"
            , wtTaskSlug = "demo"
            }
      capture <- collectDemoEvidence defaultEvidenceOptions worktree
      case capture of
        Left err -> expectationFailure ("Expected capture success, got: " <> T.unpack err)
        Right EvidenceCapture { ecScreenshotPath = mShot, ecSummary = summary } -> do
          shotPath <- maybe (expectationFailure "Missing screenshot path" >> pure "") pure mShot
          exists <- doesFileExist shotPath
          exists `shouldBe` True
          summaryShouldContain summary ["diffPixels", "pixelCount", "capture", "reference"]

summaryShouldContain :: Value -> [Text] -> Expectation
summaryShouldContain summary needles = do
  let rendered = T.toLower . T.pack . BL8.unpack $ encodePretty summary
  mapM_ (\needle -> rendered `shouldSatisfy` T.isInfixOf (T.toLower needle)) needles
