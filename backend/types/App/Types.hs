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
  , TaskHistoryPage(..)
  , TaskRunInfo(..)
  , StatusEventDTO(..)
  , ArtifactDTO(..)
  , PromptTemplateDTO(..)
  , PromptRevisionDTO(..)
  , AppSettingsDTO(..)
  , TaskCreateRequest(..)
  , TaskUpdateStatusRequest(..)
  , TaskTestCommandUpdateRequest(..)
  , SettingsUpdateRequest(..)
  , PromptUpdateRequest(..)
  , PromptResetRequest(..)
  , AgentMessageRequest(..)
  , TaskRetryRequest(..)
  , TaskSnoozeRequest(..)
  , TaskSnoozeStatus(..)
  , TaskRedirectRequest(..)
  , TaskRedirectResponse(..)
  , WorkerStatusStateDTO(..)
  , WorkerStatusDTO(..)
  , WorkerMetricDTO(..)
  , OrchestratorSnapshot(..)
  , elmDefinitions
  ) where

import Data.Aeson (FromJSON, ToJSON, Value)
import qualified Data.Aeson as A
import Data.Aeson.TypeScript.TH (TypeScript(..), deriveTypeScript)
import Data.Proxy (Proxy(..))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.TH (derivePersistField)
import Elm.Derive (defaultOptions, deriveElmDef)
import Elm.Module (DefineElm(..))
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
  | StepSpecVerification
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
  , taskSummaryIsPaused :: Bool
  , taskSummarySnoozeUntil :: Maybe UTCTime
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
  { statusEventId :: Maybe Int64
  , statusEventStep :: WorkflowStep
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
  , taskDetailTestCommand :: Text
  , taskDetailTestCommandOverride :: Maybe Text
  , taskDetailIsPaused :: Bool
  , taskDetailSnoozeUntil :: Maybe UTCTime
  , taskDetailEventNextCursor :: Maybe Int64
  }
  deriving (Show, Eq, Generic, ToJSON)

data TaskHistoryPage = TaskHistoryPage
  { taskHistoryEvents :: [StatusEventDTO]
  , taskHistoryNextCursor :: Maybe Int64
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

data TaskTestCommandUpdateRequest = TaskTestCommandUpdateRequest
  { taskTestCommand :: Maybe Text
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
  , agentMessageRole :: Maybe AgentRole
  , agentMessageNextStep :: Maybe WorkflowStep
  , agentMessageAutoResume :: Maybe Bool
  }
  deriving (Show, Eq, Generic, FromJSON)

data TaskRetryRequest = TaskRetryRequest
  { taskRetryInstructions :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON)

data TaskSnoozeRequest = TaskSnoozeRequest
  { taskSnoozeMinutes :: Int
  }
  deriving (Show, Eq, Generic, FromJSON)

data TaskSnoozeStatus = TaskSnoozeStatus
  { taskSnoozeUntil :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON)

data TaskRedirectRequest = TaskRedirectRequest
  { taskRedirectTargetTaskId :: Int
  }
  deriving (Show, Eq, Generic, FromJSON)

data TaskRedirectResponse = TaskRedirectResponse
  { taskRedirectRequeuedStep :: WorkflowStep
  , taskRedirectTargetTaskId :: Maybe Int
  , taskRedirectTargetStep :: Maybe WorkflowStep
  , taskRedirectWorkerId :: Maybe Int
  }
  deriving (Show, Eq, Generic, ToJSON)

data WorkerStatusStateDTO
  = WorkerStatusIdle
      { workerStatusIdleSince :: UTCTime
      }
  | WorkerStatusRunning
      { workerStatusTaskId :: Int64
      , workerStatusTaskTitle :: Maybe Text
      , workerStatusStep :: WorkflowStep
      , workerStatusStartedAt :: UTCTime
      }
  deriving (Show, Eq, Generic, ToJSON)

data WorkerStatusDTO = WorkerStatusDTO
  { workerStatusId :: Int
  , workerStatusState :: WorkerStatusStateDTO
  }
  deriving (Show, Eq, Generic, ToJSON)

data WorkerMetricDTO = WorkerMetricDTO
  { workerMetricId :: Int
  , workerMetricCurrentTaskId :: Maybe Int
  , workerMetricCurrentStep :: Maybe WorkflowStep
  , workerMetricStartedAt :: Maybe UTCTime
  , workerMetricLastTaskId :: Maybe Int
  , workerMetricLastStep :: Maybe WorkflowStep
  , workerMetricLastDurationSeconds :: Maybe Double
  , workerMetricLastSuccess :: Maybe Bool
  , workerMetricLastError :: Maybe Text
  , workerMetricTotalAssignments :: Int
  , workerMetricTotalBusySeconds :: Double
  }
  deriving (Show, Eq, Generic, ToJSON)

data OrchestratorSnapshot = OrchestratorSnapshot
  { snapshotActiveTasks :: [TaskSummary]
  , snapshotQueueDepth :: Int
  , snapshotWorkers :: [WorkerStatusDTO]
  , snapshotPausedTasks :: [Int]
  , snapshotWorkerMetrics :: [WorkerMetricDTO]
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
deriveTypeScript A.defaultOptions ''TaskHistoryPage
deriveTypeScript A.defaultOptions ''PromptTemplateDTO
deriveTypeScript A.defaultOptions ''PromptRevisionDTO
deriveTypeScript A.defaultOptions ''AppSettingsDTO
deriveTypeScript A.defaultOptions ''TaskCreateRequest
deriveTypeScript A.defaultOptions ''TaskUpdateStatusRequest
deriveTypeScript A.defaultOptions ''TaskTestCommandUpdateRequest
deriveTypeScript A.defaultOptions ''SettingsUpdateRequest
deriveTypeScript A.defaultOptions ''PromptUpdateRequest
deriveTypeScript A.defaultOptions ''PromptResetRequest
deriveTypeScript A.defaultOptions ''AgentMessageRequest
deriveTypeScript A.defaultOptions ''TaskRetryRequest
deriveTypeScript A.defaultOptions ''TaskRedirectRequest
deriveTypeScript A.defaultOptions ''TaskRedirectResponse
deriveTypeScript A.defaultOptions ''WorkerStatusStateDTO
deriveTypeScript A.defaultOptions ''WorkerStatusDTO
deriveTypeScript A.defaultOptions ''WorkerMetricDTO
deriveTypeScript A.defaultOptions ''OrchestratorSnapshot
deriveTypeScript A.defaultOptions ''TaskSnoozeRequest
deriveTypeScript A.defaultOptions ''TaskSnoozeStatus

deriveElmDef defaultOptions ''TaskStatus
deriveElmDef defaultOptions ''WorkflowStep
deriveElmDef defaultOptions ''AgentRole
deriveElmDef defaultOptions ''AgentSessionStatus
deriveElmDef defaultOptions ''ArtifactKind
deriveElmDef defaultOptions ''PreviewStatus
deriveElmDef defaultOptions ''TaskSummary
deriveElmDef defaultOptions ''TaskRunInfo
deriveElmDef defaultOptions ''ArtifactDTO
deriveElmDef defaultOptions ''StatusEventDTO
deriveElmDef defaultOptions ''TaskDetail
deriveElmDef defaultOptions ''TaskHistoryPage
deriveElmDef defaultOptions ''PromptTemplateDTO
deriveElmDef defaultOptions ''PromptRevisionDTO
deriveElmDef defaultOptions ''AppSettingsDTO
deriveElmDef defaultOptions ''TaskCreateRequest
deriveElmDef defaultOptions ''TaskUpdateStatusRequest
deriveElmDef defaultOptions ''TaskTestCommandUpdateRequest
deriveElmDef defaultOptions ''SettingsUpdateRequest
deriveElmDef defaultOptions ''PromptUpdateRequest
deriveElmDef defaultOptions ''PromptResetRequest
deriveElmDef defaultOptions ''AgentMessageRequest
deriveElmDef defaultOptions ''TaskRetryRequest
deriveElmDef defaultOptions ''TaskRedirectRequest
deriveElmDef defaultOptions ''TaskRedirectResponse
deriveElmDef defaultOptions ''WorkerStatusStateDTO
deriveElmDef defaultOptions ''WorkerStatusDTO
deriveElmDef defaultOptions ''WorkerMetricDTO
deriveElmDef defaultOptions ''OrchestratorSnapshot
deriveElmDef defaultOptions ''TaskSnoozeRequest
deriveElmDef defaultOptions ''TaskSnoozeStatus

elmDefinitions :: [DefineElm]
elmDefinitions =
  [ DefineElm (Proxy :: Proxy TaskStatus)
  , DefineElm (Proxy :: Proxy WorkflowStep)
  , DefineElm (Proxy :: Proxy AgentRole)
  , DefineElm (Proxy :: Proxy AgentSessionStatus)
  , DefineElm (Proxy :: Proxy ArtifactKind)
  , DefineElm (Proxy :: Proxy PreviewStatus)
  , DefineElm (Proxy :: Proxy TaskSummary)
  , DefineElm (Proxy :: Proxy TaskRunInfo)
  , DefineElm (Proxy :: Proxy ArtifactDTO)
  , DefineElm (Proxy :: Proxy StatusEventDTO)
  , DefineElm (Proxy :: Proxy TaskDetail)
  , DefineElm (Proxy :: Proxy PromptTemplateDTO)
  , DefineElm (Proxy :: Proxy PromptRevisionDTO)
  , DefineElm (Proxy :: Proxy AppSettingsDTO)
  , DefineElm (Proxy :: Proxy TaskCreateRequest)
  , DefineElm (Proxy :: Proxy TaskUpdateStatusRequest)
  , DefineElm (Proxy :: Proxy TaskTestCommandUpdateRequest)
  , DefineElm (Proxy :: Proxy SettingsUpdateRequest)
  , DefineElm (Proxy :: Proxy PromptUpdateRequest)
  , DefineElm (Proxy :: Proxy PromptResetRequest)
  , DefineElm (Proxy :: Proxy AgentMessageRequest)
  , DefineElm (Proxy :: Proxy TaskRetryRequest)
  , DefineElm (Proxy :: Proxy TaskRedirectRequest)
  , DefineElm (Proxy :: Proxy TaskRedirectResponse)
  , DefineElm (Proxy :: Proxy WorkerStatusStateDTO)
  , DefineElm (Proxy :: Proxy WorkerStatusDTO)
  , DefineElm (Proxy :: Proxy WorkerMetricDTO)
  , DefineElm (Proxy :: Proxy OrchestratorSnapshot)
  , DefineElm (Proxy :: Proxy TaskSnoozeRequest)
  , DefineElm (Proxy :: Proxy TaskSnoozeStatus)
  ]
