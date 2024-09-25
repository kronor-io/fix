{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages.Header where

import Data.Validity
import FIX.Components.Class
import FIX.Fields.BeginString
import FIX.Fields.BodyLength
import FIX.Fields.CstmApplVerID
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
import FIX.Fields.SenderCompID
import FIX.Fields.SenderLocationID
import FIX.Fields.SenderSubID
import FIX.Fields.SendingTime
import FIX.Fields.TargetCompID
import FIX.Fields.TargetLocationID
import FIX.Fields.TargetSubID
import FIX.Fields.XmlData
import FIX.Groups.Class
import FIX.Groups.HopsGroupElem
import GHC.Generics (Generic)

data Header = Header
  { headerBeginString :: !BeginString,
    headerBodyLength :: !BodyLength,
    headerMsgType :: !MsgType,
    headerSenderCompID :: !SenderCompID,
    headerTargetCompID :: !TargetCompID,
    headerOnBehalfOfCompID :: !(Maybe OnBehalfOfCompID),
    headerDeliverToCompID :: !(Maybe DeliverToCompID),
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
    headerXmlData :: !(Maybe XmlData),
    headerMessageEncoding :: !(Maybe MessageEncoding),
    headerLastMsgSeqNumProcessed :: !(Maybe LastMsgSeqNumProcessed),
    headerHopsGroup :: ![HopsGroupElem],
    headerCstmApplVerID :: !(Maybe CstmApplVerID)
  }
  deriving stock (Show, Eq, Generic)

instance Validity Header

instance IsComponent Header where
  toComponentFields ((Header {..})) =
    mconcat
      [ requiredFieldB headerBeginString,
        requiredFieldB headerBodyLength,
        requiredFieldB headerMsgType,
        requiredFieldB headerSenderCompID,
        requiredFieldB headerTargetCompID,
        optionalFieldB headerOnBehalfOfCompID,
        optionalFieldB headerDeliverToCompID,
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
        optionalFieldB headerXmlData,
        optionalFieldB headerMessageEncoding,
        optionalFieldB headerLastMsgSeqNumProcessed,
        optionalGroupB headerHopsGroup,
        optionalFieldB headerCstmApplVerID
      ]
  fromComponentFields = do
    headerBeginString <- requiredFieldP
    headerBodyLength <- requiredFieldP
    headerMsgType <- requiredFieldP
    headerSenderCompID <- requiredFieldP
    headerTargetCompID <- requiredFieldP
    headerOnBehalfOfCompID <- optionalFieldP
    headerDeliverToCompID <- optionalFieldP
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
    headerXmlData <- optionalFieldP
    headerMessageEncoding <- optionalFieldP
    headerLastMsgSeqNumProcessed <- optionalFieldP
    headerHopsGroup <- optionalGroupP
    headerCstmApplVerID <- optionalFieldP
    pure (Header {..})

makeHeader :: BeginString -> (BodyLength -> (MsgType -> (SenderCompID -> (TargetCompID -> (MsgSeqNum -> (SendingTime -> Header))))))
makeHeader headerBeginString headerBodyLength headerMsgType headerSenderCompID headerTargetCompID headerMsgSeqNum headerSendingTime =
  let headerOnBehalfOfCompID = Nothing
      headerDeliverToCompID = Nothing
      headerSecureData = Nothing
      headerSenderSubID = Nothing
      headerSenderLocationID = Nothing
      headerTargetSubID = Nothing
      headerTargetLocationID = Nothing
      headerOnBehalfOfSubID = Nothing
      headerOnBehalfOfLocationID = Nothing
      headerDeliverToSubID = Nothing
      headerDeliverToLocationID = Nothing
      headerPossDupFlag = Nothing
      headerPossResend = Nothing
      headerOrigSendingTime = Nothing
      headerXmlData = Nothing
      headerMessageEncoding = Nothing
      headerLastMsgSeqNumProcessed = Nothing
      headerHopsGroup = []
      headerCstmApplVerID = Nothing
   in (Header {..})
