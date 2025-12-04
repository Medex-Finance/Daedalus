{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module App.Evidence
  ( EvidenceCapture(..)
  , EvidenceOptions(..)
  , defaultEvidenceOptions
  , collectDemoEvidence
  ) where

import App.Worktree (WorktreeContext(..))
import Control.Applicative ((<|>))
import Codec.Picture
  ( Image
  , Pixel8
  , PixelRGBA8(..)
  , convertRGBA8
  , imageHeight
  , imageWidth
  , pixelAt
  , generateImage
  , readImage
  )
import Data.Aeson (Value, object, (.=))
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.Environment (lookupEnv, getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), makeRelative)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode, shell)

data EvidenceCapture = EvidenceCapture
  { ecSummary :: Value
  , ecScreenshotPath :: Maybe FilePath
  , ecSource :: Text
  , ecSandboxDir :: FilePath
  }
  deriving (Show, Eq)

data EvidenceOptions = EvidenceOptions
  { eoSandboxOverride :: Maybe FilePath
  , eoCaptureCommand :: Maybe Text
  , eoEnv :: [(String, String)]
  }

defaultEvidenceOptions :: EvidenceOptions
defaultEvidenceOptions =
  EvidenceOptions
    { eoSandboxOverride = Nothing
    , eoCaptureCommand = Nothing
    , eoEnv = []
    }

collectDemoEvidence :: EvidenceOptions -> WorktreeContext -> IO (Either Text EvidenceCapture)
collectDemoEvidence options WorktreeContext { wtRepoRoot, wtRoot } = do
  sandboxOverride <- lookupOptionEnv options "VERIFIER_SANDBOX_ROOT"
  let sandboxDir =
        fromMaybe (wtRepoRoot </> "demo" </> "verifier-sandbox")
          (eoSandboxOverride options <|> sandboxOverride)
      referencePath = sandboxDir </> "reference.png"
      currentPath = sandboxDir </> "current.png"
  sandboxExists <- doesDirectoryExist sandboxDir
  if not sandboxExists
    then pure $ Left (missingDirMsg sandboxDir)
    else do
      envCaptureOverride <- lookupOptionEnv options "VERIFIER_CAPTURE_COMMAND"
      let captureOverride =
            eoCaptureCommand options <|> fmap T.pack envCaptureOverride
          captureText = fmap T.unpack captureOverride
      automationOutcome <- runGeminiAutomation sandboxDir (eoEnv options)
      captureOutcome <- case automationOutcome of
        Right () -> pure (Right ())
        Left _ -> runCapturePipeline sandboxDir captureText (eoEnv options)
      case captureOutcome of
        Left err -> pure (Left err)
        Right () -> do
          hasReference <- doesFileExist referencePath
          hasCurrent <- doesFileExist currentPath
          if not hasReference || not hasCurrent
            then pure $ Left "Verifier sandbox is missing reference.png or current.png"
            else do
              capturePath <- copyCapture currentPath wtRoot
              diffResult <- compareImages referencePath capturePath
              case diffResult of
                Left err -> pure $ Left err
                Right stats -> do
                  let summary = object
                        [ "source" .= ("demo/verifier-sandbox" :: Text)
                        , "reference" .= makeRelative wtRepoRoot referencePath
                        , "capture" .= makeRelative wtRepoRoot capturePath
                        , "diffPixels" .= dsDiffPixels stats
                        , "pixelCount" .= dsPixelCount stats
                        , "diffRatio" .= dsDiffRatio stats
                        , "averageDelta" .= dsAverageDelta stats
                        , "width" .= dsWidth stats
                        , "height" .= dsHeight stats
                        ]
                  pure $ Right EvidenceCapture
                    { ecSummary = summary
                    , ecScreenshotPath = Just capturePath
                    , ecSource = "demo/verifier-sandbox"
                    , ecSandboxDir = sandboxDir
                    }

missingDirMsg :: FilePath -> Text
missingDirMsg path =
  "Verifier demo sandbox not found at " <> T.pack path <> ". Add demo/verifier-sandbox to your repo to enable automated evidence."

runCapturePipeline :: FilePath -> Maybe String -> [(String, String)] -> IO (Either Text ())
runCapturePipeline sandboxDir overrideCommand envOverrides = do
  envOverride <- lookupEnv "VERIFIER_CAPTURE_COMMAND"
  let defaultScript = sandboxDir </> "capture.sh"
  hasScript <- doesFileExist defaultScript
  let commandSpec =
        case overrideCommand <|> envOverride of
          Just cmd -> Just (shell cmd)
          Nothing ->
            if hasScript then
              Just (proc defaultScript [])
            else
              Nothing
  case commandSpec of
    Nothing -> pure (Right ())
    Just spec -> do
      baseEnv <- getEnvironment
      let mergedEnv = mergeEnv envOverrides baseEnv
      result <-
        readCreateProcessWithExitCode
          spec
            { cwd = Just sandboxDir
            , env = Just mergedEnv
            }
          ""
      case result of
        (ExitSuccess, _, _) -> pure (Right ())
        (code, out, err) ->
          pure . Left $
            T.unlines
              [ "Capture script failed"
              , "exit code: " <> T.pack (show code)
              , "stdout: " <> T.pack out
              , "stderr: " <> T.pack err
              ]

runGeminiAutomation :: FilePath -> [(String, String)] -> IO (Either Text ())
runGeminiAutomation sandboxDir envOverrides = do
  let driver = sandboxDir </> "gemini-driver.mjs"
  exists <- doesFileExist driver
  if not exists
    then pure (Left "Gemini automation driver not found; skipping")
    else do
      baseEnv <- getEnvironment
      let mergedEnv = mergeEnv envOverrides baseEnv
      result <-
        readCreateProcessWithExitCode
          (proc "node" [driver])
            { cwd = Just sandboxDir
            , env = Just mergedEnv
            }
          ""
      case result of
        (ExitSuccess, _, _) -> pure (Right ())
        (code, out, err) ->
          pure . Left $
            T.unlines
              [ "Gemini automation failed"
              , "exit code: " <> T.pack (show code)
              , "stdout: " <> T.pack out
              , "stderr: " <> T.pack err
              ]

copyCapture :: FilePath -> FilePath -> IO FilePath
copyCapture source wtRoot = do
  let evidenceDir = wtRoot </> ".orchestrator" </> "evidence"
  createDirectoryIfMissing True evidenceDir
  timestamp <- (round <$> getPOSIXTime) :: IO Integer
  let destination = evidenceDir </> ("capture-" <> show timestamp <> ".png")
  copyFile source destination
  pure destination

data DiffStats = DiffStats
  { dsWidth :: Int
  , dsHeight :: Int
  , dsPixelCount :: Int
  , dsDiffPixels :: Int
  , dsDiffRatio :: Double
  , dsAverageDelta :: Double
  }

compareImages :: FilePath -> FilePath -> IO (Either Text DiffStats)
compareImages reference capture = do
  refResult <- readImage reference
  case refResult of
    Left err -> pure $ Left (T.pack err)
    Right refDyn -> do
      capResult <- readImage capture
      case capResult of
        Left err -> pure $ Left (T.pack err)
        Right capDyn -> pure $ diff (convertRGBA8 refDyn) (convertRGBA8 capDyn)
  where
    diff :: Image PixelRGBA8 -> Image PixelRGBA8 -> Either Text DiffStats
    diff refImg capImg =
      let widthCap = imageWidth capImg
          heightCap = imageHeight capImg
          alignedRef =
            if imageWidth refImg == widthCap && imageHeight refImg == heightCap
              then refImg
              else resizeImage refImg widthCap heightCap
          coords = [ (x, y) | y <- [0 .. heightCap - 1], x <- [0 .. widthCap - 1] ]
          (different, deltaSum) = foldl' (accumulate alignedRef capImg) (0 :: Int, 0 :: Double) coords
          total = widthCap * heightCap
          ratio =
            if total == 0
              then 0
              else fromIntegral different / fromIntegral total
          avgDelta =
            if total == 0
              then 0
              else deltaSum / fromIntegral total
       in Right DiffStats
            { dsWidth = widthCap
            , dsHeight = heightCap
            , dsPixelCount = total
            , dsDiffPixels = different
            , dsDiffRatio = ratio
            , dsAverageDelta = avgDelta
            }

    resizeImage :: Image PixelRGBA8 -> Int -> Int -> Image PixelRGBA8
    resizeImage img targetW targetH =
      let srcW = imageWidth img
          srcH = imageHeight img
          scaleX = fromIntegral srcW / fromIntegral targetW
          scaleY = fromIntegral srcH / fromIntegral targetH
          sample x y =
            let sx = min (srcW - 1) (floor (fromIntegral x * scaleX))
                sy = min (srcH - 1) (floor (fromIntegral y * scaleY))
             in pixelAt img sx sy
       in generateImage sample targetW targetH

    accumulate :: Image PixelRGBA8 -> Image PixelRGBA8 -> (Int, Double) -> (Int, Int) -> (Int, Double)
    accumulate refImg capImg (diffPixels, deltaTotal) (x, y) =
      let PixelRGBA8 r1 g1 b1 a1 = pixelAt refImg x y
          PixelRGBA8 r2 g2 b2 a2 = pixelAt capImg x y
          delta = averageDelta r1 g1 b1 a1 r2 g2 b2 a2
          threshold = 8 :: Double
          isDifferent = delta > threshold
          newDiff = if isDifferent then diffPixels + 1 else diffPixels
       in (newDiff, deltaTotal + delta)

averageDelta :: Pixel8 -> Pixel8 -> Pixel8 -> Pixel8 -> Pixel8 -> Pixel8 -> Pixel8 -> Pixel8 -> Double
averageDelta r1 g1 b1 a1 r2 g2 b2 a2 =
  let channels =
        [ diffChannel r1 r2
        , diffChannel g1 g2
        , diffChannel b1 b2
        , diffChannel a1 a2
        ]
   in sum channels / fromIntegral (length channels)

diffChannel :: Pixel8 -> Pixel8 -> Double
diffChannel a b = abs (fromIntegral a - fromIntegral b)

lookupOptionEnv :: EvidenceOptions -> String -> IO (Maybe String)
lookupOptionEnv EvidenceOptions { eoEnv } key =
  case lookup key eoEnv of
    Just value -> pure (Just value)
    Nothing -> lookupEnv key

mergeEnv :: [(String, String)] -> [(String, String)] -> [(String, String)]
mergeEnv overrides base =
  let overrideKeys = map fst overrides
   in overrides <> filter (\(k, _) -> k `notElem` overrideKeys) base
