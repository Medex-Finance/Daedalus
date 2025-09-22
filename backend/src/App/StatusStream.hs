{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module App.StatusStream
  ( StatusHub(..)
  , StatusEnvelope(..)
  , newStatusHub
  , publishStatus
  , subscribeStatus
  , statusSource
  ) where

import App.Types (StatusEventDTO)
import App.Models (TaskId)
import Control.Concurrent.STM
  ( TChan
  , atomically
  , dupTChan
  , newBroadcastTChanIO
  , readTChan
  , writeTChan
  )
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Conduit (ConduitT, yield)

-- | Envelope carrying task identifier for routing.
data StatusEnvelope = StatusEnvelope
  { envelopeTaskId :: TaskId
  , envelopeEvent :: StatusEventDTO
  }

newtype StatusHub = StatusHub
  { hubChan :: TChan StatusEnvelope
  }

newStatusHub :: IO StatusHub
newStatusHub = StatusHub <$> newBroadcastTChanIO

publishStatus :: StatusHub -> StatusEnvelope -> IO ()
publishStatus StatusHub { hubChan } env = atomically $ writeTChan hubChan env

subscribeStatus :: StatusHub -> IO (TChan StatusEnvelope)
subscribeStatus StatusHub { hubChan } = atomically $ dupTChan hubChan

statusSource :: MonadIO m => TChan StatusEnvelope -> ConduitT () StatusEnvelope m ()
statusSource chan = forever $ do
  env <- liftIO $ atomically $ readTChan chan
  yield env
