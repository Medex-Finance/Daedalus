{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module App.Types
  ( TaskStatus(..)
  , WorkflowStep(..)
  , AgentRole(..)
  , AgentSessionStatus(..)
  , ArtifactKind(..)
  , PreviewStatus(..)
  , TaskSummary(..)
  , TaskDetail(..)
  , TaskRunInfo(..)
  , StatusEventDTO(..)
  , ArtifactDTO(..)
  , PromptTemplateDTO(..)
  , PromptRevisionDTO(..)
  , AppSettingsDTO(..)
  , TaskCreateRequest(..)
  , TaskUpdateStatusRequest(..)
  , SettingsUpdateRequest(..)
  , PromptUpdateRequest(..)
  , PromptResetRequest(..)
  , AgentMessageRequest(..)
  , OrchestratorSnapshot(..)
  ) where

import Data.Aeson (FromJSON, ToJSON, Value)
import qualified Data.Aeson as A
import Data.Aeson.TypeScript.TH (TypeScript(..), deriveTypeScript)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.TH (derivePersistField)
import GHC.Generics (Generic)

-- | High level task lifecycle status.
data TaskStatus
  = TaskStatusPending
  | TaskStatusDesigning
  | TaskStatusImplementing
  | TaskStatusReviewing
  | TaskStatusQa
  | TaskStatusBlocked
  | TaskStatusCompleted
  | TaskStatusDiscarded
  | TaskStatusCancelled
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data WorkflowStep
  = StepIntake
  | StepDesign
  | StepImplementation
  | StepPmReview
  | StepQaReview
  | StepFixIteration
  | StepCommit
  | StepPreview
  | StepFinalize
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data AgentRole
  = AgentRoleProjectManager
  | AgentRoleImplementer
  | AgentRoleQa
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data AgentSessionStatus
  = AgentSessionIdle
  | AgentSessionRunning
  | AgentSessionSucceeded
  | AgentSessionErrored
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data ArtifactKind
  = ArtifactDesign
  | ArtifactDiff
  | ArtifactTestLog
  | ArtifactCommitLog
  | ArtifactPreviewLog
  | ArtifactPreviewPing
  | ArtifactAgentTranscript
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data PreviewStatus
  = PreviewOffline
  | PreviewLaunching
  | PreviewOnline
  | PreviewFailed
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data TaskSummary = TaskSummary
  { taskSummaryId :: Int
  , taskSummaryTitle :: Text
  , taskSummaryStatus :: TaskStatus
  , taskSummaryRepoRoot :: Text
  , taskSummaryBranch :: Text
  , taskSummaryFeatureBranch :: Maybe Text
  , taskSummaryUpdatedAt :: UTCTime
  , taskSummaryPreviewUrl :: Maybe Text
  , taskSummaryPreviewStatus :: PreviewStatus
  }
  deriving (Show, Eq, Generic, ToJSON)

data TaskRunInfo = TaskRunInfo
  { taskRunOrdinal :: Int
  , taskRunCurrentStep :: WorkflowStep
  , taskRunPmSummary :: Maybe Text
  , taskRunCreatedAt :: UTCTime
  , taskRunUpdatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON)

data ArtifactDTO = ArtifactDTO
  { artifactKind :: ArtifactKind
  , artifactLabel :: Text
  , artifactBody :: Maybe Value
  , artifactPath :: Maybe Text
  , artifactCreatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON)

data StatusEventDTO = StatusEventDTO
  { statusEventStep :: WorkflowStep
  , statusEventMessage :: Text
  , statusEventCreatedAt :: UTCTime
  , statusEventPayload :: Maybe Value
  }
  deriving (Show, Eq, Generic, ToJSON)

data TaskDetail = TaskDetail
  { taskDetailSummary :: TaskSummary
  , taskDetailRuns :: [TaskRunInfo]
  , taskDetailEvents :: [StatusEventDTO]
  , taskDetailArtifacts :: [ArtifactDTO]
  }
  deriving (Show, Eq, Generic, ToJSON)

data PromptTemplateDTO = PromptTemplateDTO
  { promptTemplateKey :: Text
  , promptTemplateDescription :: Text
  , promptTemplateContent :: Text
  , promptTemplateVersion :: Int
  , promptTemplateUpdatedAt :: UTCTime
  , promptTemplateIsCustom :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON)

data PromptRevisionDTO = PromptRevisionDTO
  { promptRevisionKey :: Text
  , promptRevisionVersion :: Int
  , promptRevisionEditor :: Maybe Text
  , promptRevisionContent :: Text
  , promptRevisionCreatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON)

data AppSettingsDTO = AppSettingsDTO
  { settingsDefaultRepoRoot :: Text
  , settingsDefaultBranch :: Text
  , settingsTestCommand :: Text
  , settingsPreviewCommand :: Maybe Text
  , settingsInactivityMinutes :: Int
  , settingsUpdatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON)

data TaskCreateRequest = TaskCreateRequest
  { taskReqTitle :: Text
  , taskReqDescription :: Text
  , taskReqRepoRoot :: Maybe Text
  , taskReqBranch :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON)

data TaskUpdateStatusRequest = TaskUpdateStatusRequest
  { taskStatus :: TaskStatus
  }
  deriving (Show, Eq, Generic, FromJSON)

data SettingsUpdateRequest = SettingsUpdateRequest
  { settingsRepoRoot :: Text
  , settingsBranch :: Text
  , settingsUpdateTestCommand :: Text
  , settingsUpdatePreviewCommand :: Maybe Text
  , settingsUpdateInactivityMinutes :: Int
  }
  deriving (Show, Eq, Generic, FromJSON)

data PromptUpdateRequest = PromptUpdateRequest
  { promptUpdateContent :: Text
  , promptUpdateDescription :: Maybe Text
  , promptUpdateVersion :: Int
  }
  deriving (Show, Eq, Generic, FromJSON)

data PromptResetRequest = PromptResetRequest
  { promptResetKey :: Text
  }
  deriving (Show, Eq, Generic, FromJSON)

data AgentMessageRequest = AgentMessageRequest
  { agentMessage :: Text
  }
  deriving (Show, Eq, Generic, FromJSON)

data OrchestratorSnapshot = OrchestratorSnapshot
  { snapshotActiveTasks :: [TaskSummary]
  , snapshotQueueDepth :: Int
  }
  deriving (Show, Eq, Generic, ToJSON)

$(derivePersistField "TaskStatus")
$(derivePersistField "WorkflowStep")
$(derivePersistField "AgentRole")
$(derivePersistField "AgentSessionStatus")
$(derivePersistField "ArtifactKind")
$(derivePersistField "PreviewStatus")

instance TypeScript UTCTime where
  getTypeScriptType _ = "string"

deriveTypeScript A.defaultOptions ''TaskStatus
deriveTypeScript A.defaultOptions ''WorkflowStep
deriveTypeScript A.defaultOptions ''AgentRole
deriveTypeScript A.defaultOptions ''AgentSessionStatus
deriveTypeScript A.defaultOptions ''ArtifactKind
deriveTypeScript A.defaultOptions ''PreviewStatus
deriveTypeScript A.defaultOptions ''TaskSummary
deriveTypeScript A.defaultOptions ''TaskRunInfo
deriveTypeScript A.defaultOptions ''ArtifactDTO
deriveTypeScript A.defaultOptions ''StatusEventDTO
deriveTypeScript A.defaultOptions ''TaskDetail
deriveTypeScript A.defaultOptions ''PromptTemplateDTO
deriveTypeScript A.defaultOptions ''PromptRevisionDTO
deriveTypeScript A.defaultOptions ''AppSettingsDTO
deriveTypeScript A.defaultOptions ''TaskCreateRequest
deriveTypeScript A.defaultOptions ''TaskUpdateStatusRequest
deriveTypeScript A.defaultOptions ''SettingsUpdateRequest
deriveTypeScript A.defaultOptions ''PromptUpdateRequest
deriveTypeScript A.defaultOptions ''PromptResetRequest
deriveTypeScript A.defaultOptions ''AgentMessageRequest
deriveTypeScript A.defaultOptions ''OrchestratorSnapshot
