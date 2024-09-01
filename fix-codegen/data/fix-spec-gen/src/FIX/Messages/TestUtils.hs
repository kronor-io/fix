{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FIX.Messages.TestUtils (messageSpec) where

import Control.Monad
import qualified Data.ByteString as SB
import Data.Typeable
import FIX.Fields
import FIX.Messages.Class
import FIX.Messages.Envelope
import FIX.Messages.Gen ()
import FIX.Messages.Header
import Path
import Path.IO
import Test.Syd
import Test.Syd.Validity

messageSpec ::
  forall a.
  ( Show a,
    Eq a,
    GenValid a,
    IsMessage a
  ) =>
  FilePath ->
  Spec
messageSpec dir = do
  describe "fromFields" $ do
    it "roundtrips with toFields" $
      forAllValid $ \envelopePrototype -> do
        let envelope =
              fixEnvelopeCheckSum $
                fixEnvelopeBodyLength $
                  envelopePrototype
                    { envelopeHeader = (envelopeHeader envelopePrototype) {headerMsgType = messageType (Proxy :: Proxy a)}
                    }
        let rendered = toFields (envelope :: Envelope a)
        context (ppShow rendered) $ case fromFields rendered of
          Left parseErr ->
            expectationFailure $
              unlines
                [ "Failed to parse message.",
                  show parseErr
                ]
          Right envelope' -> envelope' `shouldBe` envelope

  describe "toFields" $ do
    it "renders to valid messages" $
      producesValid (toFields :: Envelope a -> [AnyField])

  scenarioDir ("test_resources/messages/" ++ dir) $ \fp -> do
    af <- resolveFile' fp
    when (fileExtension af == Just ".tagvalue") $
      it "can parse this message and roundtrip it" $ do
        contents <- SB.readFile (fromAbsFile af)
        case parseAnyFields contents of
          Left err -> expectationFailure err
          Right message -> case fromFields message of
            Left parseErr ->
              expectationFailure $
                unlines
                  [ "Could not parse message envelope from untyped message:",
                    show parseErr
                  ]
            Right a -> do
              shouldBeValid (a :: Envelope a)
              let renderedMessage = toFields a
              shouldBeValid renderedMessage
              renderedMessage `shouldBe` message
              let renderedBytes = renderAnyFields renderedMessage
              shouldBeValid renderedBytes
              renderedBytes `shouldBe` contents
