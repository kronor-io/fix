module FIX.AnyMessageSpec (spec) where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LB
import FIX.Messages
import FIX.Messages.Envelope
import FIX.Messages.Gen ()
import Test.Syd
import Test.Syd.Validity
import Text.Megaparsec as Megaparsec

spec :: Spec
spec = do
  describe "anyMessageB" $
    it "produces valid bytestrings" $
      producesValid $
        Builder.toLazyByteString . anyMessageB
  describe "anyMessageP" $
    it "roundtrips with anyMessageB" $
      forAllValid $ \anyMessage ->
        let rendered = LB.toStrict $ Builder.toLazyByteString $ anyMessageB anyMessage
         in context (show rendered) $
              case Megaparsec.parse anyMessageP "<test>" rendered of
                Left err -> expectationFailure $ errorBundlePretty err
                Right anyMessage' -> envelopeContents anyMessage' `shouldBe` envelopeContents anyMessage
