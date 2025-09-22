{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module App.Queue
  ( AppQueue(..)
  , QueueMessage(..)
  , newQueue
  , enqueue
  , dequeue
  , queueSize
  ) where

import App.Models (TaskId)
import App.Types (WorkflowStep)
import Control.Concurrent.STM
  ( TQueue
  , TVar
  , atomically
  , modifyTVar'
  , newTQueue
  , newTVar
  , readTQueue
  , readTVarIO
  , writeTQueue
  )

-- | Messages processed by the orchestrator queue.
data QueueMessage
  = QueueKickoff TaskId
  | QueueAdvance TaskId WorkflowStep
  deriving (Show, Eq)

-- | Simple STM-backed FIFO queue with depth tracking.
data AppQueue = AppQueue
  { queueInner :: TQueue QueueMessage
  , queueDepthVar :: TVar Int
  }

newQueue :: IO AppQueue
newQueue = atomically $ do
  q <- newTQueue
  depthVar <- newTVar 0
  pure AppQueue { queueInner = q, queueDepthVar = depthVar }

enqueue :: AppQueue -> QueueMessage -> IO ()
enqueue AppQueue { queueInner, queueDepthVar } msg =
  atomically $ do
    writeTQueue queueInner msg
    modifyTVar' queueDepthVar (+ 1)

-- | Blocking dequeue used by orchestrator worker.
dequeue :: AppQueue -> IO QueueMessage
dequeue AppQueue { queueInner, queueDepthVar } =
  atomically $ do
    msg <- readTQueue queueInner
    modifyTVar' queueDepthVar (max 0 . subtract 1)
    pure msg

queueSize :: AppQueue -> IO Int
queueSize AppQueue { queueDepthVar } = readTVarIO queueDepthVar
