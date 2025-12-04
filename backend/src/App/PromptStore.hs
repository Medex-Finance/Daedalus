{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module App.PromptStore
  ( ensurePromptTemplates
  , listPromptTemplates
  , getPromptTemplate
  , updatePromptTemplate
  , resetPromptTemplate
  , promptMap
  ) where

import App.Models
import qualified App.Types as DTO
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)

promptSeeds :: [(Text, Text, Text)]
promptSeeds =
  [ ( "pm_design"
    , "Project manager technical design"
    , T.unlines
        [ "You are the project manager agent. Review the new feature request.",
          "Produce a clear technical design, including requirements, plan, risks, tests.",
          "Output Markdown with sections: Summary, Requirements, Technical Plan, Tests, Risks." ]
    )
  , ( "implementer"
    , "Implementation agent"
    , T.unlines
        [ "You implement features inside a fresh git worktree.",
          "Always add or update end-to-end tests before implementation.",
          "Describe applied changes and ensure tests pass." ]
    )
  , ( "pm_verification"
    , "Project manager spec verification"
    , T.unlines
        [ "You are verifying that the implementation satisfies the original requirements.",
          "Carefully compare the current diff and artifacts against the task description and design plan.",
          "If the work is complete and correct, exit successfully with a concise approval summary.",
          "If gaps remain, exit with a failure status and list the missing items so implementation can try again." ]
    )
  , ( "verifier"
    , "Autonomous verifier agent"
    , T.unlines
        [ "You act as an automated CTO-grade verifier."
        , "Inspect the task context, acceptance criteria, diffs, tests, preview telemetry, and evidence artifacts."
        , "Cite concrete artifacts (by label) when declaring success. Fail fast if any requirement is unmet."
        , "Only approve when evidence proves the feature works end-to-end." ]
    )
  , ( "qa_reviewer"
    , "QA and security reviewer"
    , T.unlines
        [ "You review the diff for bugs or security issues.",
          "If you find problems, list each with severity, file path, and fix guidance.",
          "Confirm readiness only when you are confident." ]
    )
  ]

ensurePromptTemplates :: ConnectionPool -> IO (Map Text DTO.PromptTemplateDTO)
ensurePromptTemplates pool = runSqlPool action pool
  where
    action = do
      now <- liftIO getCurrentTime
      forM_ promptSeeds $ \(key, description, content) -> do
        upsertBy
          (UniquePromptKey key)
          PromptTemplate
            { promptTemplateKey = key
            , promptTemplateDescription = description
            , promptTemplateContent = content
            , promptTemplateVersion = 1
            , promptTemplateUpdatedAt = now
            , promptTemplateIsCustom = False
            }
          [ PromptTemplateDescription =. description
          , PromptTemplateContent =. content
          , PromptTemplateIsCustom =. False
          , PromptTemplateUpdatedAt =. now
          , PromptTemplateVersion =. 1
          ]
      templates <- selectList [] []
      pure $ promptMap templates

promptMap :: [Entity PromptTemplate] -> Map Text DTO.PromptTemplateDTO
promptMap entities = Map.fromList $ fmap toPair entities
  where
    toPair (Entity _ tmpl) =
      ( promptTemplateKey tmpl
      , templateToDTO tmpl
      )

listPromptTemplates :: ConnectionPool -> IO [DTO.PromptTemplateDTO]
listPromptTemplates pool = runSqlPool action pool
  where
    action = fmap (fmap (templateToDTO . entityVal)) $ selectList [] []

getPromptTemplate :: ConnectionPool -> Text -> IO (Maybe DTO.PromptTemplateDTO)
getPromptTemplate pool key =
  runSqlPool action pool
  where
    action = do
      res <- selectList [PromptTemplateKey ==. key] []
      pure $ case res of
        (Entity _ tmpl : _) -> Just (templateToDTO tmpl)
        _ -> Nothing

updatePromptTemplate :: ConnectionPool -> Text -> DTO.PromptUpdateRequest -> Maybe Text -> IO (Either Text DTO.PromptTemplateDTO)
updatePromptTemplate pool key req editor =
  runSqlPool (action req) pool
  where
    action DTO.PromptUpdateRequest
      { DTO.promptUpdateContent = newContent
      , DTO.promptUpdateDescription = newDescription
      , DTO.promptUpdateVersion = incomingVersion
      } = do
      existing <- selectList [PromptTemplateKey ==. key] []
      case existing of
        [] -> pure $ Left "Prompt template not found"
        (Entity pid tmpl : _) -> do
          if promptTemplateVersion tmpl /= incomingVersion
            then pure $ Left "Prompt template version mismatch"
            else do
              now <- liftIO getCurrentTime
              let newDesc = fromMaybe (promptTemplateDescription tmpl) newDescription
                  newVersion = promptTemplateVersion tmpl + 1
              update pid
                [ PromptTemplateContent =. newContent
                , PromptTemplateDescription =. newDesc
                , PromptTemplateVersion =. newVersion
                , PromptTemplateUpdatedAt =. now
                , PromptTemplateIsCustom =. True
                ]
              _ <- insert PromptRevision
                { promptRevisionTemplateKey = key
                , promptRevisionVersion = newVersion
                , promptRevisionEditor = editor
                , promptRevisionContent = newContent
                , promptRevisionCreatedAt = now
                }
              pure . Right $ DTO.PromptTemplateDTO
                { DTO.promptTemplateKey = key
                , DTO.promptTemplateDescription = newDesc
                , DTO.promptTemplateContent = newContent
                , DTO.promptTemplateVersion = newVersion
                , DTO.promptTemplateUpdatedAt = now
                , DTO.promptTemplateIsCustom = True
                }

resetPromptTemplate :: ConnectionPool -> Text -> Maybe Text -> IO (Either Text DTO.PromptTemplateDTO)
resetPromptTemplate pool key editor = runSqlPool action pool
  where
    lookupDefault k = lookup k [ (k', (d, c)) | (k', d, c) <- promptSeeds ]
    action = do
      case lookupDefault key of
        Nothing -> pure $ Left "No default seed for prompt"
        Just (description, content) -> do
          now <- liftIO getCurrentTime
          existing <- selectList [PromptTemplateKey ==. key] []
          case existing of
            [] -> do
              _ <- insert PromptTemplate
                { promptTemplateKey = key
                , promptTemplateDescription = description
                , promptTemplateContent = content
                , promptTemplateVersion = 1
                , promptTemplateUpdatedAt = now
                , promptTemplateIsCustom = False
                }
              pure ()
            (Entity pid _ : _) ->
              update pid
                [ PromptTemplateDescription =. description
                , PromptTemplateContent =. content
                , PromptTemplateIsCustom =. False
                , PromptTemplateVersion =. 1
                , PromptTemplateUpdatedAt =. now
                ]
          _ <- insert PromptRevision
            { promptRevisionTemplateKey = key
            , promptRevisionVersion = 1
            , promptRevisionEditor = editor
            , promptRevisionContent = content
            , promptRevisionCreatedAt = now
            }
          pure . Right $
            DTO.PromptTemplateDTO
              { DTO.promptTemplateKey = key
              , DTO.promptTemplateDescription = description
              , DTO.promptTemplateContent = content
              , DTO.promptTemplateVersion = 1
              , DTO.promptTemplateUpdatedAt = now
              , DTO.promptTemplateIsCustom = False
              }

templateToDTO :: PromptTemplate -> DTO.PromptTemplateDTO
templateToDTO PromptTemplate { promptTemplateKey = key, promptTemplateDescription = description, promptTemplateContent = content, promptTemplateVersion = version, promptTemplateUpdatedAt = updatedAt, promptTemplateIsCustom = isCustom } =
  DTO.PromptTemplateDTO
    { DTO.promptTemplateKey = key
    , DTO.promptTemplateDescription = description
    , DTO.promptTemplateContent = content
    , DTO.promptTemplateVersion = version
    , DTO.promptTemplateUpdatedAt = updatedAt
    , DTO.promptTemplateIsCustom = isCustom
    }
