{-# LANGUAGE TypeApplications #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.FieldsSpec where

import FIX.Core.TestUtils
import FIX.Fields.BeginString
import FIX.Fields.BodyLength
import FIX.Fields.CheckSum
import FIX.Fields.DeliverToCompID
import FIX.Fields.DeliverToLocationID
import FIX.Fields.DeliverToSubID
import FIX.Fields.EncodedText
import FIX.Fields.EncryptMethod
import FIX.Fields.Gen ()
import FIX.Fields.HeartBtInt
import FIX.Fields.HopCompID
import FIX.Fields.HopRefID
import FIX.Fields.HopSendingTime
import FIX.Fields.LastMsgSeqNumProcessed
import FIX.Fields.MaxMessageSize
import FIX.Fields.MessageEncoding
import FIX.Fields.MsgDirection
import FIX.Fields.MsgSeqNum
import FIX.Fields.MsgType
import FIX.Fields.NextExpectedMsgSeqNum
import FIX.Fields.NoHops
import FIX.Fields.NoMsgTypes
import FIX.Fields.OnBehalfOfCompID
import FIX.Fields.OnBehalfOfLocationID
import FIX.Fields.OnBehalfOfSubID
import FIX.Fields.OrigSendingTime
import FIX.Fields.Password
import FIX.Fields.PossDupFlag
import FIX.Fields.PossResend
import FIX.Fields.RawData
import FIX.Fields.RefMsgType
import FIX.Fields.ResetSeqNumFlag
import FIX.Fields.SecureData
import FIX.Fields.SenderCompID
import FIX.Fields.SenderLocationID
import FIX.Fields.SenderSubID
import FIX.Fields.SendingTime
import FIX.Fields.Signature
import FIX.Fields.SignatureLength
import FIX.Fields.TargetCompID
import FIX.Fields.TargetLocationID
import FIX.Fields.TargetSubID
import FIX.Fields.TestMessageIndicator
import FIX.Fields.TestReqID
import FIX.Fields.Text
import FIX.Fields.Username
import FIX.Fields.XmlData
import Test.Syd

spec :: Spec
spec = do
  fieldSpec @BeginString
  fieldSpec @BodyLength
  fieldSpec @CheckSum
  fieldSpec @MsgSeqNum
  fieldSpec @MsgType
  fieldSpec @PossDupFlag
  fieldSpec @SenderCompID
  fieldSpec @SenderSubID
  fieldSpec @SendingTime
  fieldSpec @TargetCompID
  fieldSpec @TargetSubID
  fieldSpec @Text
  fieldSpec @Signature
  fieldSpec @SecureData
  fieldSpec @SignatureLength
  fieldSpec @RawData
  fieldSpec @PossResend
  fieldSpec @EncryptMethod
  fieldSpec @HeartBtInt
  fieldSpec @TestReqID
  fieldSpec @OnBehalfOfCompID
  fieldSpec @OnBehalfOfSubID
  fieldSpec @OrigSendingTime
  fieldSpec @DeliverToCompID
  fieldSpec @DeliverToSubID
  fieldSpec @ResetSeqNumFlag
  fieldSpec @SenderLocationID
  fieldSpec @TargetLocationID
  fieldSpec @OnBehalfOfLocationID
  fieldSpec @DeliverToLocationID
  fieldSpec @XmlData
  fieldSpec @MessageEncoding
  fieldSpec @EncodedText
  fieldSpec @LastMsgSeqNumProcessed
  fieldSpec @RefMsgType
  fieldSpec @MaxMessageSize
  fieldSpec @NoMsgTypes
  fieldSpec @MsgDirection
  fieldSpec @TestMessageIndicator
  fieldSpec @Username
  fieldSpec @Password
  fieldSpec @NoHops
  fieldSpec @HopCompID
  fieldSpec @HopSendingTime
  fieldSpec @HopRefID
  fieldSpec @NextExpectedMsgSeqNum
