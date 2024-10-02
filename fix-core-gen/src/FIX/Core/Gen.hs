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

instance GenValid Qty

instance GenValid LocalMktDate

instance GenValid UTCTimestamp where
  genValid = mkUTCTimestamp <$> genValid

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
