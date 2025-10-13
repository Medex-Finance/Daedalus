{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module App.Queue
  ( AppQueue(..)
  , QueueMessage(..)
  , newQueue
  , enqueueGlobal
  , enqueueForWorker
  , dequeueForWorker
  , queueSize
  ) where

import App.Models (TaskId)
import App.Types (WorkflowStep)
import Control.Concurrent.STM
  ( STM
  , TQueue
  , TVar
  , atomically
  , modifyTVar'
  , newTQueue
  , newTVar
  , readTQueue
  , readTVar
  , readTVarIO
  , tryReadTQueue
  , writeTQueue
  , writeTVar
  )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | Messages processed by the orchestrator queue.
data QueueMessage
  = QueueKickoff TaskId
  | QueueAdvance TaskId WorkflowStep
  deriving (Show, Eq)

-- | Queue structure that supports worker-specific routing while
-- maintaining a global backlog depth.
data AppQueue = AppQueue
  { queueGlobal :: TQueue QueueMessage
  , queueDepthVar :: TVar Int
  , queueWorkerQueues :: TVar (Map Int (TQueue QueueMessage))
  }

newQueue :: IO AppQueue
newQueue = atomically $ do
  globalQ <- newTQueue
  depthVar <- newTVar 0
  workerMap <- newTVar Map.empty
  pure AppQueue
    { queueGlobal = globalQ
    , queueDepthVar = depthVar
    , queueWorkerQueues = workerMap
    }

ensureWorkerQueue :: TVar (Map Int (TQueue QueueMessage)) -> Int -> STM (TQueue QueueMessage)
ensureWorkerQueue queueWorkerQueues workerId = do
  queues <- readTVar queueWorkerQueues
  case Map.lookup workerId queues of
    Just q -> pure q
    Nothing -> do
      q <- newTQueue
      writeTVar queueWorkerQueues (Map.insert workerId q queues)
      pure q

enqueueGlobal :: AppQueue -> QueueMessage -> IO ()
enqueueGlobal AppQueue { queueGlobal, queueDepthVar } msg =
  atomically $ do
    writeTQueue queueGlobal msg
    modifyTVar' queueDepthVar (+ 1)

enqueueForWorker :: AppQueue -> Int -> QueueMessage -> IO ()
enqueueForWorker AppQueue { queueDepthVar, queueWorkerQueues } workerId msg =
  atomically $ do
    workerQueue <- ensureWorkerQueue queueWorkerQueues workerId
    writeTQueue workerQueue msg
    modifyTVar' queueDepthVar (+ 1)

-- | Blocking dequeue that prefers worker-specific messages if present,
-- otherwise falls back to the shared global queue.
dequeueForWorker :: AppQueue -> Int -> IO QueueMessage
dequeueForWorker AppQueue { queueGlobal, queueDepthVar, queueWorkerQueues } workerId =
  atomically $ do
    workerQueue <- ensureWorkerQueue queueWorkerQueues workerId
    mLocal <- tryReadTQueue workerQueue
    msg <- case mLocal of
      Just localMsg -> pure localMsg
      Nothing -> readTQueue queueGlobal
    modifyTVar' queueDepthVar (max 0 . subtract 1)
    pure msg

queueSize :: AppQueue -> IO Int
queueSize AppQueue { queueDepthVar } = readTVarIO queueDepthVar
