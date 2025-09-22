{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module App.Models
  ( module App.Models
  , module Database.Persist
  , module Database.Persist.Class
  , module Database.Persist.Sql
  , module Database.Persist.TH
  ) where

import App.Types
  ( AgentRole(..)
  , AgentSessionStatus(..)
  , ArtifactKind(..)
  , PreviewStatus(..)
  , TaskStatus(..)
  , WorkflowStep(..)
  )
import Data.Aeson (Value)
import qualified Data.Aeson as A
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime)
import Database.Persist
import Database.Persist.Class
import Database.Persist.Sql
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Task
    title Text
    description Text
    status TaskStatus
    repoRoot Text
    branch Text
    featureBranch Text Maybe
    previewUrl Text Maybe
    previewStatus PreviewStatus
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    deriving Show Eq

TaskRun
    taskId TaskId
    ordinal Int
    currentStep WorkflowStep
    pmSummary Text Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    deriving Show Eq

AgentSession
    taskRunId TaskRunId
    role AgentRole
    promptKey Text
    status AgentSessionStatus
    lastError Text Maybe
    logPath Text Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    deriving Show Eq

StatusEvent
    taskId TaskId
    step WorkflowStep
    message Text
    payload Value Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    deriving Show Eq

Artifact
    taskId TaskId
    kind ArtifactKind
    label Text
    content Value Maybe
    path Text Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    deriving Show Eq

AppSettings
    defaultRepoRoot Text
    defaultBranch Text
    testCommand Text
    previewCommand Text Maybe
    inactivityTimeoutMinutes Int default=5
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    deriving Show Eq

PromptTemplate
    key Text
    description Text
    content Text
    version Int
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    isCustom Bool default=FALSE
    UniquePromptKey key
    deriving Show Eq

PromptRevision
    templateKey Text
    version Int
    editor Text Maybe
    content Text
    createdAt UTCTime default=CURRENT_TIMESTAMP
    deriving Show Eq

PreviewProcess
    taskId TaskId
    port Int
    pid Int Maybe
    status PreviewStatus
    lastHealthCheck UTCTime Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    deriving Show Eq
  |]

instance PersistField Value where
  toPersistValue = PersistText . TE.decodeUtf8 . BL.toStrict . A.encode
  fromPersistValue (PersistText t) =
    first T.pack $ A.eitherDecodeStrict' (TE.encodeUtf8 t)
  fromPersistValue (PersistByteString bs) =
    first T.pack $ A.eitherDecodeStrict' bs
  fromPersistValue other = Left $ "Expected JSON Value, found: " <> T.pack (show other)

instance PersistFieldSql Value where
  sqlType _ = SqlString
