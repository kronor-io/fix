{-# LANGUAGE TypeApplications #-}

module FIXSpec (spec) where

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

  describe "LogonMessage" $
    describe "fromMessage" $ do
      it "roundtrips with toMessage" $
        forAllValid $ \a -> do
          let rendered = toMessage (a :: LogonMessage)
          context (ppShow rendered) $ case fromMessage rendered of
            Nothing -> expectationFailure "Failed to parse message."
            Just a' -> a' `shouldBe` a
      it "renders to valid messages" $
        producesValid (toMessage :: LogonMessage -> Message)
