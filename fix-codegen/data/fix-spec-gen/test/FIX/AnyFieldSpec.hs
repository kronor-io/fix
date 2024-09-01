module FIX.AnyFieldSpec (spec) where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LB
import FIX.Fields
import FIX.Fields.Gen ()
import Test.Syd
import Test.Syd.Validity
import Text.Megaparsec as Megaparsec

spec :: Spec
spec = do
  describe "anyFieldB" $ do
    it "produces valid bytestrings" $
      producesValid $
        Builder.toLazyByteString . anyFieldB
  describe "anyFieldP" $ do
    it "roundtrips with anyFieldB" $
      forAllValid $ \anyField ->
        let rendered = LB.toStrict $ Builder.toLazyByteString $ anyFieldB anyField
         in context (show rendered) $
              case Megaparsec.parse anyFieldP "<test>" rendered of
                Left err -> expectationFailure $ errorBundlePretty err
                Right anyField' -> anyField' `shouldBe` anyField
