{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FIX.Components.TestUtils (componentSpec) where

import FIX.Components.Class
import FIX.Components.Gen ()
import FIX.Core
import Test.Syd
import Test.Syd.Validity

componentSpec ::
  forall a.
  ( Show a,
    Eq a,
    GenValid a,
    IsComponent a
  ) =>
  Spec
componentSpec = do
  describe "fromComponentFields" $ do
    it "roundtrips with toComponentFields" $
      forAllValid $ \a -> do
        let rendered = toComponentFields (a :: a)
        context (ppShow rendered) $ case runComponentP rendered fromComponentFields of
          Left parseErr ->
            expectationFailure $
              unlines
                [ "Failed to parse fields to component.",
                  show parseErr
                ]
          Right a' -> a' `shouldBe` a

  describe "toComponentFields" $ do
    it "renders to valid lists of fields" $
      producesValid (toComponentFields :: a -> [Field])
