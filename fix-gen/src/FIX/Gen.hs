{-# OPTIONS_GHC -Wno-orphans #-}

module FIX.Gen where

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.GenValidity
import Data.GenValidity.ByteString
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Word
import FIX
import Test.QuickCheck

genNonSOHWord8 :: Gen Word8
genNonSOHWord8 = genValid `suchThat` (/= 1)

genSimpleValue :: Gen ByteString
genSimpleValue = genStrictByteStringBy genNonSOHWord8 `suchThat` (not . SB.null)

genDataValue :: Gen ByteString
genDataValue = genValid `suchThat` (not . SB.null)

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

instance GenValid Message

instance GenValid MessageHeader

instance GenValid MessageTrailer

instance (GenValid a) => GenValid (Envelope a)

instance GenValid BeginString

instance GenValid CheckSum where
  genValid = fmap CheckSum $ do
    w1 <- genNonSOHWord8
    w2 <- genNonSOHWord8
    w3 <- genNonSOHWord8
    pure $ SB.pack [w1, w2, w3]

instance GenValid BodyLength where
  genValid = BodyLength <$> choose (0, 9999)

instance GenValid MessageType

instance GenValid MessageSequenceNumber

instance GenValid SenderCompId

instance GenValid TargetCompId

instance GenValid TestRequestId

instance GenValid SendingTime where
  genValid = SendingTime . mkImpreciseUTCTime <$> genValid

instance GenValid EncryptionMethod

instance GenValid HeartbeatInterval

instance GenValid HeartbeatMessage

instance GenValid LogonMessage
