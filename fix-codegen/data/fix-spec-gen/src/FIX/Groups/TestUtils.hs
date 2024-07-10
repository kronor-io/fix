{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FIX.Groups.TestUtils (groupSpec) where

import Data.Typeable
import FIX.Components.Gen ()
import FIX.Groups.Class
import Test.Syd
import Test.Syd.Validity

groupSpec ::
  forall a.
  ( IsGroupElement a,
    Validity (GroupNumField a),
    Show (GroupNumField a)
  ) =>
  Spec
groupSpec = do
  describe "countGroupNum" $ do
    it "roundtrips with mkGroupNum" $
      forAllValid $ \w -> do
        let p = Proxy :: Proxy a
            rendered = mkGroupNum p w
        context (ppShow rendered) $ countGroupNum p rendered `shouldBe` w

  describe "mkGroupNum" $ do
    it "renders to valid fields" $
      producesValid (mkGroupNum (Proxy :: Proxy a))
