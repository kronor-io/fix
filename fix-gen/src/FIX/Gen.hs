{-# OPTIONS_GHC -Wno-orphans #-}

module FIX.Gen where

import qualified Data.ByteString as SB
import Data.GenValidity
import Data.GenValidity.ByteString
import FIX
import Test.QuickCheck

instance GenValid Message where
  genValid =
    Message
      <$> genListOf
        ( (,)
            <$> genValid
            <*> (genStrictByteStringBy (genValid `suchThat` (/= 1)) `suchThat` (not . SB.null))
        )

instance GenValid LogonMessage

instance GenValid HeartbeatMessage
