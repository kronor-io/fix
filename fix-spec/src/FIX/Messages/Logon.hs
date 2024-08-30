{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages.Logon where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.EncryptMethod
import FIX.Fields.HeartBtInt
import FIX.Fields.MaxMessageSize
import FIX.Fields.MsgType
import FIX.Fields.NextExpectedMsgSeqNum
import FIX.Fields.Password
import FIX.Fields.RawData
import FIX.Fields.RawDataLength
import FIX.Fields.ResetSeqNumFlag
import FIX.Fields.TestMessageIndicator
import FIX.Fields.Username
import FIX.Groups.Class
import FIX.Groups.MsgTypes
import FIX.Messages.Class
import GHC.Generics (Generic)

-- | MessageSpec
--   { messageName = "Logon"
--   , messageType = "A"
--   , messageCategory = "admin"
--   , messagePieces =
--       [ MessagePieceField "EncryptMethod" True
--       , MessagePieceField "HeartBtInt" True
--       , MessagePieceField "RawDataLength" False
--       , MessagePieceField "RawData" False
--       , MessagePieceField "ResetSeqNumFlag" False
--       , MessagePieceField "NextExpectedMsgSeqNum" False
--       , MessagePieceField "MaxMessageSize" False
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "NoMsgTypes"
--             , groupPieces =
--                 [ MessagePieceField "RefMsgType" True
--                 , MessagePieceField "MsgDirection" False
--                 ]
--             }
--           False
--       , MessagePieceField "TestMessageIndicator" False
--       , MessagePieceField "Username" False
--       , MessagePieceField "Password" False
--       ]
--   }
data Logon = Logon
  { logonEncryptMethod :: !EncryptMethod,
    logonHeartBtInt :: !HeartBtInt,
    logonRawDataLength :: !(Maybe RawDataLength),
    logonRawData :: !(Maybe RawData),
    logonResetSeqNumFlag :: !(Maybe ResetSeqNumFlag),
    logonNextExpectedMsgSeqNum :: !(Maybe NextExpectedMsgSeqNum),
    logonMaxMessageSize :: !(Maybe MaxMessageSize),
    logonMsgTypes :: ![MsgTypes],
    logonTestMessageIndicator :: !(Maybe TestMessageIndicator),
    logonUsername :: !(Maybe Username),
    logonPassword :: !(Maybe Password)
  }
  deriving stock (Show, Eq, Generic)

instance Validity Logon

instance IsComponent Logon where
  toComponentFields ((Logon {..})) =
    concat
      [ requiredFieldB logonEncryptMethod,
        requiredFieldB logonHeartBtInt,
        optionalFieldB logonRawDataLength,
        optionalFieldB logonRawData,
        optionalFieldB logonResetSeqNumFlag,
        optionalFieldB logonNextExpectedMsgSeqNum,
        optionalFieldB logonMaxMessageSize,
        optionalGroupB logonMsgTypes,
        optionalFieldB logonTestMessageIndicator,
        optionalFieldB logonUsername,
        optionalFieldB logonPassword
      ]
  fromComponentFields = do
    logonEncryptMethod <- requiredFieldP
    logonHeartBtInt <- requiredFieldP
    logonRawDataLength <- optionalFieldP
    logonRawData <- optionalFieldP
    logonResetSeqNumFlag <- optionalFieldP
    logonNextExpectedMsgSeqNum <- optionalFieldP
    logonMaxMessageSize <- optionalFieldP
    logonMsgTypes <- optionalGroupP
    logonTestMessageIndicator <- optionalFieldP
    logonUsername <- optionalFieldP
    logonPassword <- optionalFieldP
    pure (Logon {..})

instance IsMessage Logon where
  messageType Proxy = MsgTypeLogon
