{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FIX.Core.TestUtils
  ( fieldTypeSpec,
    fieldTypeExampleSpec,
    fieldSpec,
    messageSpec,
    goldenMessageSpec,
  )
where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.Typeable
import FIX.Core
import FIX.Core.Gen ()
import Path
import Path.IO
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Utils (nameOf)

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
fieldSpec =
  describe (nameOf @a) $ do
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
    IsMessage MessageType a
  ) =>
  Spec
messageSpec =
  describe (nameOf @a) $ do
    genValidSpec @a
    describe "fromMessage" $ do
      it "roundtrips with toMessage" $
        forAllValid $ \envelopePrototype -> do
          let envelope =
                fixEnvelopeCheckSum $
                  fixEnvelopeBodyLength $
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

goldenMessageSpec ::
  forall a.
  ( Show a,
    GenValid a,
    IsMessage MessageType a
  ) =>
  FilePath ->
  Spec
goldenMessageSpec dir =
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
