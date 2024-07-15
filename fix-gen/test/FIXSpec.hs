{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FIXSpec (spec) where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
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
          let rendered = renderMessage message
          rendered `shouldBe` contents

  fieldTypeSpec @ByteString
  fieldTypeSpec @Int
  fieldTypeSpec @Word

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
          Nothing -> expectationFailure "Failed to parse field type value."
          Just a' -> a' `shouldBe` a
  describe "toValue" $ do
    it "renders to valid messages" $
      producesValid (toValue :: a -> ByteString)

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
          Nothing -> expectationFailure "Failed to parse field."
          Just a' -> a' `shouldBe` a
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
      forAllValid $ \a -> do
        let rendered = toMessage (a :: a)
        context (ppShow rendered) $ case fromMessage (messageFields rendered) of
          Nothing -> expectationFailure "Failed to parse message."
          Just a' -> a' `shouldBe` a

  describe "toMessage" $ do
    it "renders to valid messages" $
      producesValid (toMessage :: a -> Message)

  scenarioDir ("test_resources/messages/" ++ dir) $ \fp -> do
    af <- resolveFile' fp
    when (fileExtension af == Just ".tagvalue") $
      it "can parse this message and roundtrip it" $ do
        contents <- SB.readFile (fromAbsFile af)
        case parseMessage contents of
          Left err -> expectationFailure err
          Right message -> case fromMessage (messageFields message) of
            Nothing -> expectationFailure "Could not parse typed message from untyped message"
            Just a -> do
              shouldBeValid (a :: a)
              -- TODO include these tests too:
              -- let renderedMessage = toMessage a
              -- renderedMessage `shouldBe` message
              -- let renderedBytes = renderMessage renderedMessage
              -- renderedBytes `shouldBe` content
              pure ()
