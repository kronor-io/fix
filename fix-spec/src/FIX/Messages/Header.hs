{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages.Header where

import Data.Maybe
import Data.Validity
import FIX.Components.Class
import FIX.Fields.BeginString
import FIX.Fields.BodyLength
import FIX.Fields.DeliverToCompID
import FIX.Fields.DeliverToLocationID
import FIX.Fields.DeliverToSubID
import FIX.Fields.LastMsgSeqNumProcessed
import FIX.Fields.MessageEncoding
import FIX.Fields.MsgSeqNum
import FIX.Fields.MsgType
import FIX.Fields.OnBehalfOfCompID
import FIX.Fields.OnBehalfOfLocationID
import FIX.Fields.OnBehalfOfSubID
import FIX.Fields.OrigSendingTime
import FIX.Fields.PossDupFlag
import FIX.Fields.PossResend
import FIX.Fields.SecureData
import FIX.Fields.SecureDataLen
import FIX.Fields.SenderCompID
import FIX.Fields.SenderLocationID
import FIX.Fields.SenderSubID
import FIX.Fields.SendingTime
import FIX.Fields.TargetCompID
import FIX.Fields.TargetLocationID
import FIX.Fields.TargetSubID
import FIX.Fields.XmlData
import FIX.Fields.XmlDataLen
import GHC.Generics (Generic)

data Header = Header
  { headerBeginString :: !BeginString,
    headerBodyLength :: !BodyLength,
    headerMsgType :: !MsgType,
    headerSenderCompID :: !SenderCompID,
    headerTargetCompID :: !TargetCompID,
    headerOnBehalfOfCompID :: !(Maybe OnBehalfOfCompID),
    headerDeliverToCompID :: !(Maybe DeliverToCompID),
    headerSecureDataLen :: !(Maybe SecureDataLen),
    headerSecureData :: !(Maybe SecureData),
    headerMsgSeqNum :: !MsgSeqNum,
    headerSenderSubID :: !(Maybe SenderSubID),
    headerSenderLocationID :: !(Maybe SenderLocationID),
    headerTargetSubID :: !(Maybe TargetSubID),
    headerTargetLocationID :: !(Maybe TargetLocationID),
    headerOnBehalfOfSubID :: !(Maybe OnBehalfOfSubID),
    headerOnBehalfOfLocationID :: !(Maybe OnBehalfOfLocationID),
    headerDeliverToSubID :: !(Maybe DeliverToSubID),
    headerDeliverToLocationID :: !(Maybe DeliverToLocationID),
    headerPossDupFlag :: !(Maybe PossDupFlag),
    headerPossResend :: !(Maybe PossResend),
    headerSendingTime :: !SendingTime,
    headerOrigSendingTime :: !(Maybe OrigSendingTime),
    headerXmlDataLen :: !(Maybe XmlDataLen),
    headerXmlData :: !(Maybe XmlData),
    headerMessageEncoding :: !(Maybe MessageEncoding),
    headerLastMsgSeqNumProcessed :: !(Maybe LastMsgSeqNumProcessed)
  }
  deriving stock (Show, Eq, Generic)

instance Validity Header

instance IsComponent Header where
  toComponentFields ((Header {..})) =
    catMaybes
      [ requiredFieldB headerBeginString,
        requiredFieldB headerBodyLength,
        requiredFieldB headerMsgType,
        requiredFieldB headerSenderCompID,
        requiredFieldB headerTargetCompID,
        optionalFieldB headerOnBehalfOfCompID,
        optionalFieldB headerDeliverToCompID,
        optionalFieldB headerSecureDataLen,
        optionalFieldB headerSecureData,
        requiredFieldB headerMsgSeqNum,
        optionalFieldB headerSenderSubID,
        optionalFieldB headerSenderLocationID,
        optionalFieldB headerTargetSubID,
        optionalFieldB headerTargetLocationID,
        optionalFieldB headerOnBehalfOfSubID,
        optionalFieldB headerOnBehalfOfLocationID,
        optionalFieldB headerDeliverToSubID,
        optionalFieldB headerDeliverToLocationID,
        optionalFieldB headerPossDupFlag,
        optionalFieldB headerPossResend,
        requiredFieldB headerSendingTime,
        optionalFieldB headerOrigSendingTime,
        optionalFieldB headerXmlDataLen,
        optionalFieldB headerXmlData,
        optionalFieldB headerMessageEncoding,
        optionalFieldB headerLastMsgSeqNumProcessed
      ]
  fromComponentFields = do
    headerBeginString <- requiredFieldP
    headerBodyLength <- requiredFieldP
    headerMsgType <- requiredFieldP
    headerSenderCompID <- requiredFieldP
    headerTargetCompID <- requiredFieldP
    headerOnBehalfOfCompID <- optionalFieldP
    headerDeliverToCompID <- optionalFieldP
    headerSecureDataLen <- optionalFieldP
    headerSecureData <- optionalFieldP
    headerMsgSeqNum <- requiredFieldP
    headerSenderSubID <- optionalFieldP
    headerSenderLocationID <- optionalFieldP
    headerTargetSubID <- optionalFieldP
    headerTargetLocationID <- optionalFieldP
    headerOnBehalfOfSubID <- optionalFieldP
    headerOnBehalfOfLocationID <- optionalFieldP
    headerDeliverToSubID <- optionalFieldP
    headerDeliverToLocationID <- optionalFieldP
    headerPossDupFlag <- optionalFieldP
    headerPossResend <- optionalFieldP
    headerSendingTime <- requiredFieldP
    headerOrigSendingTime <- optionalFieldP
    headerXmlDataLen <- optionalFieldP
    headerXmlData <- optionalFieldP
    headerMessageEncoding <- optionalFieldP
    headerLastMsgSeqNumProcessed <- optionalFieldP
    pure (Header {..})
