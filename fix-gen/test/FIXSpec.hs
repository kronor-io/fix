{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FIXSpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import FIX
import FIX.Gen ()
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

  describe "TestRequestId" $ do
    fieldSpec @TestRequestId
  describe "HeartbeatMessage" $ do
    messageSpec @HeartbeatMessage
  describe "LogonMessage" $ do
    messageSpec @LogonMessage

messageSpec ::
  forall a.
  ( Show a,
    Eq a,
    GenValid a,
    IsMessage a
  ) =>
  Spec
messageSpec =
  describe "fromMessage" $ do
    it "roundtrips with toMessage" $
      forAllValid $ \a -> do
        let rendered = toMessage (a :: a)
        context (ppShow rendered) $ case fromMessage rendered of
          Nothing -> expectationFailure "Failed to parse message."
          Just a' -> a' `shouldBe` a
    it "renders to valid messages" $
      producesValid (toMessage :: a -> Message)

fieldSpec ::
  forall a.
  ( Show a,
    Eq a,
    GenValid a,
    IsField a
  ) =>
  Spec
fieldSpec =
  describe "fromValue" $ do
    it "roundtrips with toValue" $
      forAllValid $ \a -> do
        let rendered = toValue (a :: a)
        context (ppShow rendered) $ case fromValue rendered of
          Nothing -> expectationFailure "Failed to parse message."
          Just a' -> a' `shouldBe` a
    it "renders to valid messages" $
      producesValid (toValue :: a -> ByteString)
