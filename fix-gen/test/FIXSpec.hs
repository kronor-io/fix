{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FIXSpec (spec) where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.Time
import Data.Typeable
import FIX
import FIX.Gen ()
import Path
import Path.IO
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

  fieldTypeSpec @ByteString
  fieldTypeSpec @Int
  fieldTypeSpec @Word
  describe "UTCTime" $ do
    fieldTypeExampleSpec
      "20190605-11:57:29.363"
      (UTCTime (fromGregorian 2019 06 05) (timeOfDayToTime (TimeOfDay 11 57 29.363)))
    fieldTypeExampleSpec
      "20190605-11:57:29.360"
      (UTCTime (fromGregorian 2019 06 05) (timeOfDayToTime (TimeOfDay 11 57 29.360)))
    fieldTypeSpec @UTCTime

  describe "BeginString" $ do
    fieldSpec @BeginString
  describe "BodyLength" $ do
    fieldSpec @BodyLength
  describe "CheckSum" $ do
    fieldSpec @CheckSum
  describe "MessageSequenceNumber" $ do
    fieldSpec @MessageSequenceNumber
  describe "MessageType" $ do
    fieldSpec @MessageType
  describe "SenderCompId" $ do
    fieldSpec @SenderCompId
  describe "TargetCompId" $ do
    fieldSpec @TargetCompId
  describe "SendingTime" $ do
    fieldSpec @SendingTime
  describe "EncryptionMethod" $ do
    fieldSpec @EncryptionMethod
  describe "HeartbeatInterval" $ do
    fieldSpec @HeartbeatInterval
  describe "TestRequestId" $ do
    fieldSpec @TestRequestId

  describe "HeartbeatMessage" $ do
    messageSpec @HeartbeatMessage "heartbeat"
  describe "LogonMessage" $ do
    messageSpec @LogonMessage "logon"

fieldTypeSpec ::
  forall a.
  ( Show a,
    Eq a,
    GenValid a,
    Typeable a,
    IsFieldType a
  ) =>
  Spec
fieldTypeSpec = do
  genValidSpec @a
  describe "fromValue" $ do
    it "roundtrips with toValue" $
      forAllValid $ \a -> do
        let rendered = toValue (a :: a)
        context (ppShow rendered) $ case fromValue rendered of
          Left err ->
            expectationFailure $
              unlines
                [ "Failed to parse field type value:",
                  err
                ]
          Right a' -> a' `shouldBe` a
  describe "toValue" $ do
    it "renders to valid messages" $
      producesValid (toValue :: a -> ByteString)

fieldTypeExampleSpec ::
  forall a.
  (Show a, Eq a, IsFieldType a) =>
  ByteString ->
  a ->
  Spec
fieldTypeExampleSpec rendered parsed = do
  it (unwords ["can render", show parsed, "to", show rendered]) $
    toValue parsed `shouldBe` rendered
  it (unwords ["can parse", show rendered, "to", show parsed]) $
    case fromValue rendered of
      Left err -> expectationFailure $ "Failed to parse: " <> err
      Right actual -> actual `shouldBe` parsed

fieldSpec ::
  forall a.
  ( Show a,
    Eq a,
    GenValid a,
    Typeable a,
    IsField a
  ) =>
  Spec
fieldSpec = do
  describe "fieldFromValue" $ do
    genValidSpec @a
    it "roundtrips with fieldToValue" $
      forAllValid $ \a -> do
        let rendered = fieldToValue (a :: a)
        context (ppShow rendered) $ case fieldFromValue rendered of
          Left err ->
            expectationFailure $
              unlines
                [ "Failed to parse field.",
                  err
                ]
          Right a' -> a' `shouldBe` a
  describe "fieldToValue" $ do
    it "renders to valid messages" $
      producesValid (fieldToValue :: a -> ByteString)

messageSpec ::
  forall a.
  ( Show a,
    Eq a,
    GenValid a,
    Typeable a,
    IsMessage a
  ) =>
  FilePath ->
  Spec
messageSpec dir = do
  genValidSpec @a
  describe "fromMessage" $ do
    it "roundtrips with toMessage" $
      forAllValid $ \envelopePrototype -> do
        let envelope =
              fixEnvelopeCheckSum $
                envelopePrototype
                  { envelopeHeader = (envelopeHeader envelopePrototype) {messageHeaderMessageType = messageType (Proxy :: Proxy a)}
                  }
        let rendered = toMessage (envelope :: Envelope a)
        context (ppShow rendered) $ case fromMessage rendered of
          Left parseErr ->
            expectationFailure $
              unlines
                [ "Failed to parse message.",
                  show parseErr
                ]
          Right envelope' -> envelope' `shouldBe` envelope

  describe "toMessage" $ do
    it "renders to valid messages" $
      producesValid (toMessage :: Envelope a -> Message)

  scenarioDir ("test_resources/messages/" ++ dir) $ \fp -> do
    af <- resolveFile' fp
    when (fileExtension af == Just ".tagvalue") $
      it "can parse this message and roundtrip it" $ do
        contents <- SB.readFile (fromAbsFile af)
        case parseMessage contents of
          Left err -> expectationFailure err
          Right message -> case fromMessage message of
            Left parseErr ->
              expectationFailure $
                unlines
                  [ "Could not parse message envelope from untyped message:",
                    show parseErr
                  ]
            Right a -> do
              shouldBeValid (a :: Envelope a)
              let renderedMessage = toMessage a
              shouldBeValid renderedMessage
              renderedMessage `shouldBe` message
              let renderedBytes = renderMessage renderedMessage
              shouldBeValid renderedBytes
              renderedBytes `shouldBe` contents
