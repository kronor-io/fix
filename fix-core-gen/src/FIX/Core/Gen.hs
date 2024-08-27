{-# OPTIONS_GHC -Wno-orphans #-}

module FIX.Core.Gen where

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.GenValidity
import Data.GenValidity.ByteString
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Word
import FIX.Core
import Test.QuickCheck

genNonSOHWord8 :: Gen Word8
genNonSOHWord8 = genValid `suchThat` (/= 1)

genSimpleValue :: Gen ByteString
genSimpleValue = genStrictByteStringBy genNonSOHWord8 `suchThat` (not . SB.null)

instance GenValid SimpleBytes where
  genValid = SimpleBytes <$> genSimpleValue

genDataValue :: Gen ByteString
genDataValue = genValid `suchThat` (not . SB.null)

instance GenValid DataBytes where
  genValid = DataBytes <$> genDataValue

instance GenValid Value where
  genValid =
    oneof
      [ ValueSimple <$> genSimpleValue,
        ValueData <$> genDataValue
      ]

instance GenValid Field where
  genValid = do
    tag <- genValid
    Field tag
      <$> ( if tagIsLen tag
              then ValueData <$> genDataValue
              else ValueSimple <$> genSimpleValue
          )

instance GenValid UTCTimestamp where
  genValid = mkUTCTimestamp <$> genValid

instance GenValid Message
