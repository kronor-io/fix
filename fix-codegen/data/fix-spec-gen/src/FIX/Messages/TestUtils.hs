{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FIX.Messages.TestUtils where

import Control.Monad
import qualified Data.ByteString as SB
import Data.Typeable
import FIX.Core
import FIX.Messages.Class
import FIX.Messages.Envelope
import FIX.Messages.Gen ()
import FIX.Messages.Header
import Path
import Path.IO
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Utils

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
messageSpec dir =
  describe (nameOf @a) $ do
    genValidSpec @a
    describe "fromMessage" $ do
      it "roundtrips with toMessage" $
        forAllValid $ \envelopePrototype -> do
          let envelope =
                fixEnvelopeCheckSum $
                  fixEnvelopeBodyLength $
                    envelopePrototype
                      { envelopeHeader = (envelopeHeader envelopePrototype) {headerMsgType = messageType (Proxy :: Proxy a)}
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
