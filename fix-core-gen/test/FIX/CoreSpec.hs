{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FIX.CoreSpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.Time
import FIX.Core
import FIX.Core.Gen ()
import FIX.Core.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @Message
  describe "renderMessage" $ do
    it "roundtrips with parseMessage" $
      forAllValid $ \message ->
        parseMessage (renderMessage message)
          `shouldBe` Right message
  describe "parseMessage" $ do
    it "can roundtrip this message" $ do
      contents <- SB.readFile "test_resources/messages/example.tagvalue"
      case parseMessage contents of
        Left err -> expectationFailure err
        Right message -> do
          shouldBeValid message
          let rendered = renderMessage message
          rendered `shouldBe` contents

  describe "ByteString" $ do
    fieldTypeSpec @ByteString
  describe "Int" $ do
    fieldTypeSpec @Int
  describe "Word" $ do
    fieldTypeSpec @Word
  describe "UTCTime" $ do
    fieldTypeExampleSpec
      "20190605-11:57:29.363"
      (UTCTime (fromGregorian 2019 06 05) (timeOfDayToTime (TimeOfDay 11 57 29.363)))
    fieldTypeExampleSpec
      "20190605-11:57:29.360"
      (UTCTime (fromGregorian 2019 06 05) (timeOfDayToTime (TimeOfDay 11 57 29.360)))
    fieldTypeSpec @UTCTime

  fieldSpec @BeginString
  fieldSpec @BodyLength
  fieldSpec @CheckSum
  fieldSpec @MessageSequenceNumber
  fieldSpec @MessageType
  fieldSpec @SenderCompId
  fieldSpec @SenderSubId
  fieldSpec @TargetCompId
  fieldSpec @SendingTime
  fieldSpec @EncryptionMethod
  fieldSpec @HeartbeatInterval
  fieldSpec @TestRequestId
  fieldSpec @Password

  messageSpec @HeartbeatMessage "heartbeat"
  messageSpec @LogonMessage "logon"
