{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module App.Gemini
  ( GeminiVerdict(..)
  , runGeminiReview
  ) where

import App.Models
  ( Entity(..)
  , TaskAcceptanceCriterion(..)
  )
import Control.Exception (SomeException, try)
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , (.:)
  , (.:?)
  , (.=)
  , (.!=)
  , eitherDecodeStrict'
  , encode
  , object
  , withObject
  )
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Network.HTTP.Client
  ( RequestBody(..)
  , httpLbs
  , method
  , newManager
  , parseRequest
  , requestBody
  , requestHeaders
  , responseBody
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (lookupEnv)

data GeminiVerdict = GeminiVerdict
  { gvPassed :: Bool
  , gvSummary :: Text
  , gvViolations :: [Text]
  }
  deriving (Show, Eq)

instance FromJSON GeminiVerdict where
  parseJSON = withObject "GeminiVerdict" $ \obj ->
    GeminiVerdict
      <$> obj .: "pass"
      <*> obj .: "summary"
      <*> obj .:? "violations" .!= []

instance ToJSON GeminiVerdict where
  toJSON GeminiVerdict { gvPassed, gvSummary, gvViolations } =
    object
      [ "pass" .= gvPassed
      , "summary" .= gvSummary
      , "violations" .= gvViolations
      ]

runGeminiReview
  :: Maybe FilePath
  -> Value -- ^ evidence summary
  -> [Entity TaskAcceptanceCriterion]
  -> IO (Either Text GeminiVerdict)
runGeminiReview Nothing _ _ = pure (Left "Screenshot missing from evidence bundle")
runGeminiReview (Just screenshotPath) evidenceSummary criteria = do
  fakeMode <- lookupEnv "GEMINI_FAKE_MODE"
  case fakeMode of
    Just "1" -> pure $ Right GeminiVerdict
      { gvPassed = True
      , gvSummary = "Fake Gemini verdict (pass)."
      , gvViolations = []
      }
    _ -> do
      mApiKey <- lookupEnv "GEMINI_API_KEY"
      case mApiKey of
        Nothing -> pure (Left "GEMINI_API_KEY not set; cannot call Gemini evaluator")
        Just apiKey -> do
          bodyResult <- buildRequestBody screenshotPath evidenceSummary criteria
          case bodyResult of
            Left err -> pure (Left err)
            Right body -> do
              modelName <- lookupEnv "GEMINI_MODEL"
              let model = maybe "gemini-1.5-flash" id modelName
                  endpoint = "https://generativelanguage.googleapis.com/v1beta/models/" <> model <> ":generateContent?key=" <> apiKey
              reqEither <- try @SomeException (parseRequest endpoint)
              case reqEither of
                Left _ -> pure $ Left "Failed to parse Gemini endpoint URL"
                Right req0 -> do
                  let req =
                        req0
                          { method = "POST"
                          , requestHeaders =
                              [ ("Content-Type", "application/json")
                              ]
                          , requestBody = RequestBodyLBS body
                          }
                  manager <- newManager tlsManagerSettings
                  responseResult <- try @SomeException (httpLbs req manager)
                  case responseResult of
                    Left _ ->
                      pure $ Left "Gemini request failed (network error)"
                    Right resp ->
                      pure $ parseGeminiResponse (responseBody resp)

buildRequestBody
  :: FilePath
  -> Value
  -> [Entity TaskAcceptanceCriterion]
  -> IO (Either Text BL.ByteString)
buildRequestBody screenshotPath evidenceSummary criteria = do
  imgResult <- try @SomeException (BL.readFile screenshotPath)
  case imgResult of
    Left _ -> pure $ Left "Unable to read screenshot for Gemini request"
    Right imgBytes -> do
      let base64Data = TE.decodeUtf8 . B64.encode . BL.toStrict $ imgBytes
          formattedCriteria = formatCriteria criteria
          summaryText = TE.decodeUtf8 . BL.toStrict $ encodePretty evidenceSummary
          prompt = T.intercalate "\n"
            [ "You are an autonomous QA verifier."
            , "Acceptance criteria:"
            , formattedCriteria
            , ""
            , "Evidence summary JSON:"
            , summaryText
            , ""
            , "Evaluate whether ALL criteria pass by inspecting the attached screenshot."
            , "Respond with strict JSON: {\"pass\":true|false,\"summary\":\"...\",\"violations\":[\"...\"]}."
            ]
          body =
            object
              [ "contents" .=
                  [ object
                      [ "parts" .=
                          [ object ["text" .= prompt]
                          , object
                              [ "inline_data" .= object
                                  [ "mime_type" .= ("image/png" :: Text)
                                  , "data" .= base64Data
                                  ]
                              ]
                          ]
                      ]
                  ]
              ]
      pure (Right (encode body))

parseGeminiResponse :: BL.ByteString -> Either Text GeminiVerdict
parseGeminiResponse raw =
  case eitherDecodeStrictText =<< extractTextPayload raw of
    Left err -> Left ("Gemini response parse error: " <> err)
    Right verdict -> Right verdict

extractTextPayload :: BL.ByteString -> Either Text Text
extractTextPayload raw =
  case eitherDecodeStrict' (BL.toStrict raw) of
    Left err -> Left (T.pack err)
    Right (Object obj) ->
      case KeyMap.lookup "candidates" obj of
        Just (Array arr) ->
          case V.toList arr of
            (Object cand : _) ->
              case KeyMap.lookup "content" cand of
                Just (Object contentObj) ->
                  case KeyMap.lookup "parts" contentObj of
                    Just (Array partsArr) ->
                      case V.toList partsArr of
                        (Object partObj : _) ->
                          case KeyMap.lookup "text" partObj of
                            Just (String txt) -> Right txt
                            _ -> Left "Gemini response missing text content"
                        _ -> Left "Gemini response missing parts array"
                    _ -> Left "Gemini response missing parts field"
                _ -> Left "Gemini response missing content field"
            _ -> Left "Gemini response missing candidates entries"
        _ -> Left "Gemini response missing candidates"
    _ -> Left "Unexpected Gemini response payload"

eitherDecodeStrictText :: FromJSON a => Text -> Either Text a
eitherDecodeStrictText txt =
  case eitherDecodeStrict' (TE.encodeUtf8 txt) of
    Left err -> Left (T.pack err)
    Right val -> Right val

formatCriteria :: [Entity TaskAcceptanceCriterion] -> Text
formatCriteria crits =
  if null crits
    then "(No acceptance criteria provided)"
    else T.unlines $
      zipWith
        (\idx (Entity _ TaskAcceptanceCriterion { taskAcceptanceCriterionBody }) ->
          let idxText = T.pack (show (idx :: Int))
           in idxText <> ". " <> taskAcceptanceCriterionBody
        )
        [1 ..]
        crits
