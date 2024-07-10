module FIXSpec (spec) where

import FIX
import FIX.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "renderMessage" $
    it "roundtrips with parseMessage" $
      forAllValid $ \message ->
        parseMessage (renderMessage message) `shouldBe` Right message
