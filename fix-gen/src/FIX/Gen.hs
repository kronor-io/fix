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

instance GenValid BeginString

instance GenValid BodyLength where
  genValid = BodyLength <$> choose (0, 9999)

instance GenValid MessageType

instance GenValid SenderCompId

instance GenValid TargetCompId

instance GenValid TestRequestId

instance GenValid EncryptionMethod

instance GenValid HeartbeatInterval

instance GenValid HeartbeatMessage

instance GenValid LogonMessage
