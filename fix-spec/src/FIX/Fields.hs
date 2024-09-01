{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields (AnyField (..), anyFieldP, anyFieldB, module X) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as ByteString
import Data.Validity
import Data.Void (Void)
import FIX.Core
import FIX.Fields.BeginString as X
import FIX.Fields.BodyLength as X
import FIX.Fields.CheckSum as X
import FIX.Fields.DeliverToCompID as X
import FIX.Fields.DeliverToLocationID as X
import FIX.Fields.DeliverToSubID as X
import FIX.Fields.EncryptMethod as X
import FIX.Fields.HeartBtInt as X
import FIX.Fields.HopCompID as X
import FIX.Fields.HopRefID as X
import FIX.Fields.HopSendingTime as X
import FIX.Fields.LastMsgSeqNumProcessed as X
import FIX.Fields.MaxMessageSize as X
import FIX.Fields.MessageEncoding as X
import FIX.Fields.MsgDirection as X
import FIX.Fields.MsgSeqNum as X
import FIX.Fields.MsgType as X
import FIX.Fields.NextExpectedMsgSeqNum as X
import FIX.Fields.NoHops as X
import FIX.Fields.NoMsgTypes as X
import FIX.Fields.OnBehalfOfCompID as X
import FIX.Fields.OnBehalfOfLocationID as X
import FIX.Fields.OnBehalfOfSubID as X
import FIX.Fields.OrigSendingTime as X
import FIX.Fields.Password as X
import FIX.Fields.PossDupFlag as X
import FIX.Fields.PossResend as X
import FIX.Fields.RawData as X
import FIX.Fields.RawDataLength as X
import FIX.Fields.RefMsgType as X
import FIX.Fields.ResetSeqNumFlag as X
import FIX.Fields.SecureData as X
import FIX.Fields.SecureDataLen as X
import FIX.Fields.SenderCompID as X
import FIX.Fields.SenderLocationID as X
import FIX.Fields.SenderSubID as X
import FIX.Fields.SendingTime as X
import FIX.Fields.Signature as X
import FIX.Fields.SignatureLength as X
import FIX.Fields.TargetCompID as X
import FIX.Fields.TargetLocationID as X
import FIX.Fields.TargetSubID as X
import FIX.Fields.TestMessageIndicator as X
import FIX.Fields.TestReqID as X
import FIX.Fields.Username as X
import FIX.Fields.XmlData as X
import FIX.Fields.XmlDataLen as X
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Byte.Lexer

data AnyField
  = SomeBeginString !BeginString
  | SomeBodyLength !BodyLength
  | SomeCheckSum !CheckSum
  | SomeMsgSeqNum !MsgSeqNum
  | SomeMsgType !MsgType
  | SomePossDupFlag !PossDupFlag
  | SomeSenderCompID !SenderCompID
  | SomeSenderSubID !SenderSubID
  | SomeSendingTime !SendingTime
  | SomeTargetCompID !TargetCompID
  | SomeTargetSubID !TargetSubID
  | SomeSignature !Signature
  | SomeSecureDataLen !SecureDataLen
  | SomeSecureData !SecureData
  | SomeSignatureLength !SignatureLength
  | SomeRawDataLength !RawDataLength
  | SomeRawData !RawData
  | SomePossResend !PossResend
  | SomeEncryptMethod !EncryptMethod
  | SomeHeartBtInt !HeartBtInt
  | SomeTestReqID !TestReqID
  | SomeOnBehalfOfCompID !OnBehalfOfCompID
  | SomeOnBehalfOfSubID !OnBehalfOfSubID
  | SomeOrigSendingTime !OrigSendingTime
  | SomeDeliverToCompID !DeliverToCompID
  | SomeDeliverToSubID !DeliverToSubID
  | SomeResetSeqNumFlag !ResetSeqNumFlag
  | SomeSenderLocationID !SenderLocationID
  | SomeTargetLocationID !TargetLocationID
  | SomeOnBehalfOfLocationID !OnBehalfOfLocationID
  | SomeDeliverToLocationID !DeliverToLocationID
  | SomeXmlDataLen !XmlDataLen
  | SomeXmlData !XmlData
  | SomeMessageEncoding !MessageEncoding
  | SomeLastMsgSeqNumProcessed !LastMsgSeqNumProcessed
  | SomeRefMsgType !RefMsgType
  | SomeMaxMessageSize !MaxMessageSize
  | SomeNoMsgTypes !NoMsgTypes
  | SomeMsgDirection !MsgDirection
  | SomeTestMessageIndicator !TestMessageIndicator
  | SomeUsername !Username
  | SomePassword !Password
  | SomeNoHops !NoHops
  | SomeHopCompID !HopCompID
  | SomeHopSendingTime !HopSendingTime
  | SomeHopRefID !HopRefID
  | SomeNextExpectedMsgSeqNum !NextExpectedMsgSeqNum
  deriving stock (Show, Eq, Generic)

instance Validity AnyField

anyFieldB :: AnyField -> ByteString.Builder
anyFieldB = \case
  SomeBeginString f -> fieldB f
  SomeBodyLength f -> fieldB f
  SomeCheckSum f -> fieldB f
  SomeMsgSeqNum f -> fieldB f
  SomeMsgType f -> fieldB f
  SomePossDupFlag f -> fieldB f
  SomeSenderCompID f -> fieldB f
  SomeSenderSubID f -> fieldB f
  SomeSendingTime f -> fieldB f
  SomeTargetCompID f -> fieldB f
  SomeTargetSubID f -> fieldB f
  SomeSignature f -> fieldB f
  SomeSecureDataLen f -> fieldB f
  SomeSecureData f -> fieldB f
  SomeSignatureLength f -> fieldB f
  SomeRawDataLength f -> fieldB f
  SomeRawData f -> fieldB f
  SomePossResend f -> fieldB f
  SomeEncryptMethod f -> fieldB f
  SomeHeartBtInt f -> fieldB f
  SomeTestReqID f -> fieldB f
  SomeOnBehalfOfCompID f -> fieldB f
  SomeOnBehalfOfSubID f -> fieldB f
  SomeOrigSendingTime f -> fieldB f
  SomeDeliverToCompID f -> fieldB f
  SomeDeliverToSubID f -> fieldB f
  SomeResetSeqNumFlag f -> fieldB f
  SomeSenderLocationID f -> fieldB f
  SomeTargetLocationID f -> fieldB f
  SomeOnBehalfOfLocationID f -> fieldB f
  SomeDeliverToLocationID f -> fieldB f
  SomeXmlDataLen f -> fieldB f
  SomeXmlData f -> fieldB f
  SomeMessageEncoding f -> fieldB f
  SomeLastMsgSeqNumProcessed f -> fieldB f
  SomeRefMsgType f -> fieldB f
  SomeMaxMessageSize f -> fieldB f
  SomeNoMsgTypes f -> fieldB f
  SomeMsgDirection f -> fieldB f
  SomeTestMessageIndicator f -> fieldB f
  SomeUsername f -> fieldB f
  SomePassword f -> fieldB f
  SomeNoHops f -> fieldB f
  SomeHopCompID f -> fieldB f
  SomeHopSendingTime f -> fieldB f
  SomeHopRefID f -> fieldB f
  SomeNextExpectedMsgSeqNum f -> fieldB f

anyFieldP :: Parsec Void ByteString AnyField
anyFieldP = do
  tag <- decimal
  let fp :: forall f. (IsField f) => Parsec Void ByteString f
      fp = fieldP tag
  case tag of
    8 -> SomeBeginString <$> fp
    9 -> SomeBodyLength <$> fp
    10 -> SomeCheckSum <$> fp
    34 -> SomeMsgSeqNum <$> fp
    35 -> SomeMsgType <$> fp
    43 -> SomePossDupFlag <$> fp
    49 -> SomeSenderCompID <$> fp
    50 -> SomeSenderSubID <$> fp
    52 -> SomeSendingTime <$> fp
    56 -> SomeTargetCompID <$> fp
    57 -> SomeTargetSubID <$> fp
    89 -> SomeSignature <$> fp
    90 -> SomeSecureDataLen <$> fp
    91 -> SomeSecureData <$> fp
    93 -> SomeSignatureLength <$> fp
    95 -> SomeRawDataLength <$> fp
    96 -> SomeRawData <$> fp
    97 -> SomePossResend <$> fp
    98 -> SomeEncryptMethod <$> fp
    108 -> SomeHeartBtInt <$> fp
    112 -> SomeTestReqID <$> fp
    115 -> SomeOnBehalfOfCompID <$> fp
    116 -> SomeOnBehalfOfSubID <$> fp
    122 -> SomeOrigSendingTime <$> fp
    128 -> SomeDeliverToCompID <$> fp
    129 -> SomeDeliverToSubID <$> fp
    141 -> SomeResetSeqNumFlag <$> fp
    142 -> SomeSenderLocationID <$> fp
    143 -> SomeTargetLocationID <$> fp
    144 -> SomeOnBehalfOfLocationID <$> fp
    145 -> SomeDeliverToLocationID <$> fp
    212 -> SomeXmlDataLen <$> fp
    213 -> SomeXmlData <$> fp
    347 -> SomeMessageEncoding <$> fp
    369 -> SomeLastMsgSeqNumProcessed <$> fp
    372 -> SomeRefMsgType <$> fp
    383 -> SomeMaxMessageSize <$> fp
    384 -> SomeNoMsgTypes <$> fp
    385 -> SomeMsgDirection <$> fp
    464 -> SomeTestMessageIndicator <$> fp
    553 -> SomeUsername <$> fp
    554 -> SomePassword <$> fp
    627 -> SomeNoHops <$> fp
    628 -> SomeHopCompID <$> fp
    629 -> SomeHopSendingTime <$> fp
    630 -> SomeHopRefID <$> fp
    789 -> SomeNextExpectedMsgSeqNum <$> fp
    _ -> undefined
