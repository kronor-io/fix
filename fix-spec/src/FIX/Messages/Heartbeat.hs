{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages.Heartbeat where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.MsgType
import FIX.Fields.TestReqID
import FIX.Messages.Class
import GHC.Generics (Generic)

-- | MessageSpec
--   { messageName = "Heartbeat"
--   , messageType = "0"
--   , messageCategory = "admin"
--   , messagePieces = [ MessagePieceField "TestReqID" False ]
--   }
data Heartbeat = Heartbeat {heartbeatTestReqID :: !(Maybe TestReqID)}
  deriving stock (Show, Eq, Generic)

instance Validity Heartbeat

instance IsComponent Heartbeat where
  toComponentFields ((Heartbeat {..})) = catMaybes [optionalFieldB heartbeatTestReqID]
  fromComponentFields = do
    heartbeatTestReqID <- optionalFieldP
    pure (Heartbeat {..})

instance IsMessage Heartbeat where
  messageType Proxy = MsgTypeHeartbeat
