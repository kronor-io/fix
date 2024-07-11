{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FIXSpec (spec) where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
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

  describe "TestRequestId" $ do
    fieldSpec @TestRequestId
  describe "EncryptionMethod" $ do
    fieldSpec @EncryptionMethod

  describe "HeartbeatMessage" $ do
    messageSpec @HeartbeatMessage "heartbeat"
  describe "LogonMessage" $ do
    messageSpec @LogonMessage "logon"

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

messageSpec ::
  forall a.
  ( Show a,
    Eq a,
    GenValid a,
    IsMessage a
  ) =>
  FilePath ->
  Spec
messageSpec dir =
  describe "fromMessage" $ do
    it "roundtrips with toMessage" $
      forAllValid $ \a -> do
        let rendered = toMessage (a :: a)
        context (ppShow rendered) $ case fromMessage rendered of
          Nothing -> expectationFailure "Failed to parse message."
          Just a' -> a' `shouldBe` a
    it "renders to valid messages" $
      producesValid (toMessage :: a -> Message)

    scenarioDir ("test_resources/messages/" ++ dir) $ \fp -> do
      af <- resolveFile' fp
      when (fileExtension af == Just ".tagvalue") $
        it "can parse this message and roundtrip it" $ do
          contents <- SB.readFile (fromAbsFile af)
          case parseMessage contents of
            Left err -> expectationFailure err
            Right message -> case fromMessage message of
              Nothing -> expectationFailure "Could not parse typed message from untyped message"
              Just a -> do
                shouldBeValid (a :: a)
                -- TODO include these tests too:
                -- let renderedMessage = toMessage a
                -- renderedMessage `shouldBe` message
                -- let renderedBytes = renderMessage renderedMessage
                -- renderedBytes `shouldBe` content
                pure ()
