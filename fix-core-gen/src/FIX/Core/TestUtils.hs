{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FIX.Core.TestUtils
  ( fieldTypeSpec,
    fieldTypeExampleSpec,
    fieldSpec,
  )
where

import Data.ByteString (ByteString)
import Data.Typeable
import FIX.Core
import FIX.Core.Gen ()
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
