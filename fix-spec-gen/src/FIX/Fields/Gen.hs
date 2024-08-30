{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.Gen where

import Data.GenValidity
import Data.GenValidity.ByteString ()
import FIX.Core.Gen ()
import FIX.Fields

instance GenValid AnyField

instance GenValid BeginString

instance GenValid BodyLength

instance GenValid CheckSum

instance GenValid MsgSeqNum

instance GenValid MsgType

instance GenValid PossDupFlag

instance GenValid SenderCompID

instance GenValid SenderSubID

instance GenValid SendingTime

instance GenValid TargetCompID

instance GenValid TargetSubID

instance GenValid Signature

instance GenValid SecureDataLen

instance GenValid SecureData

instance GenValid SignatureLength

instance GenValid RawDataLength

instance GenValid RawData

instance GenValid PossResend

instance GenValid EncryptMethod

instance GenValid HeartBtInt

instance GenValid TestReqID

instance GenValid OnBehalfOfCompID

instance GenValid OnBehalfOfSubID

instance GenValid OrigSendingTime

instance GenValid DeliverToCompID

instance GenValid DeliverToSubID

instance GenValid ResetSeqNumFlag

instance GenValid SenderLocationID

instance GenValid TargetLocationID

instance GenValid OnBehalfOfLocationID

instance GenValid DeliverToLocationID

instance GenValid XmlDataLen

instance GenValid XmlData

instance GenValid MessageEncoding

instance GenValid LastMsgSeqNumProcessed

instance GenValid RefMsgType

instance GenValid MaxMessageSize

instance GenValid NoMsgTypes

instance GenValid MsgDirection

instance GenValid TestMessageIndicator

instance GenValid Username

instance GenValid Password

instance GenValid NoHops

instance GenValid HopCompID

instance GenValid HopSendingTime

instance GenValid HopRefID

instance GenValid NextExpectedMsgSeqNum
