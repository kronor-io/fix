{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FIX.Core where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder as ByteString
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Text ()
import Data.Validity.Time ()
import Data.Void
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer
import Text.Read (readMaybe)

type Tag = Word

class IsFieldType a where
  toValue :: a -> ByteString
  fromValue :: ByteString -> Either String a

instance IsFieldType Bool where
  toValue = \case
    True -> "Y"
    False -> "N"
  fromValue = \case
    "Y" -> Right True
    "N" -> Right False
    s -> Left $ "Could not Read Bool: " <> show s

instance IsFieldType ByteString where
  toValue = id
  fromValue = Right

-- | Bytes of data fields
-- These are nonempty.
newtype DataBytes = DataBytes {unDataBytes :: ByteString}
  deriving (Show, Eq, Generic)

instance Validity DataBytes where
  validate sb@(DataBytes value) =
    mconcat
      [ genericValidate sb,
        declare "The value is nonempty" $
          not $
            SB.null value
      ]

instance IsFieldType DataBytes where
  toValue = unDataBytes
  fromValue = prettyValidate . DataBytes

-- | Bytes of non-data fields
-- These are nonempty and not '\SOH'.
newtype SimpleBytes = SimpleBytes {unSimpleBytes :: ByteString}
  deriving (Show, Eq, Generic)

instance Validity SimpleBytes where
  validate sb@(SimpleBytes value) =
    mconcat
      [ genericValidate sb,
        declare "The value is nonempty" $
          not $
            SB.null value,
        decorateList (SB.unpack value) $ \w ->
          declare "The value is not '\\SOH'" $
            w /= 1
      ]

instance IsFieldType SimpleBytes where
  toValue = unSimpleBytes
  fromValue = prettyValidate . SimpleBytes

instance IsFieldType Word where
  toValue = TE.encodeUtf8 . T.pack . show
  fromValue sb =
    let s = T.unpack $ TE.decodeLatin1 sb
     in case readMaybe s of
          Nothing -> Left $ "Could not Read Word from String: " <> show s
          Just w -> Right w

instance IsFieldType Int where
  toValue = TE.encodeUtf8 . T.pack . show
  fromValue sb =
    let s = T.unpack $ TE.decodeLatin1 sb
     in case readMaybe s of
          Nothing -> Left $ "Could not Read Int from String: " <> show s
          Just w -> Right w

newtype UTCTimestamp = UTCTimestamp {unUTCTimestamp :: UTCTime}
  deriving (Show, Eq, Generic)

instance Validity UTCTimestamp where
  validate uts@(UTCTimestamp t) =
    mconcat
      [ genericValidate uts,
        validateImpreciseUTCTime t
      ]

instance IsFieldType UTCTimestamp where
  toValue = TE.encodeUtf8 . T.pack . formatTime defaultTimeLocale "%Y%m%d-%H:%M:%S%03Q" . unUTCTimestamp
  fromValue sb =
    let s = T.unpack $ TE.decodeLatin1 sb
     in case parseTimeM False defaultTimeLocale utcTimeFormatExact s
          <|> parseTimeM False defaultTimeLocale utcTimeFormat s of
          Nothing -> Left $ "Could not Read UTCTime from String: " <> show s
          Just u -> Right (UTCTimestamp u)

utcTimeFormat :: String
utcTimeFormat = "%Y%m%d-%X"

utcTimeFormatExact :: String
utcTimeFormatExact = "%Y%m%d-%H:%M:%S%Q"

mkUTCTimestamp :: UTCTime -> UTCTimestamp
mkUTCTimestamp u = UTCTimestamp $ u {utctDayTime = fromIntegral (floor (utctDayTime u) :: Word)}

validateImpreciseUTCTime :: UTCTime -> Validation
validateImpreciseUTCTime = validateImpreciseLocalTime . utcToLocalTime utc

validateImpreciseLocalTime :: LocalTime -> Validation
validateImpreciseLocalTime lt =
  let tod = localTimeOfDay lt
   in validateImpreciseTimeOfDay tod

validateImpreciseTimeOfDay :: TimeOfDay -> Validation
validateImpreciseTimeOfDay tod =
  declare "The number of seconds has at most three digits" $
    let sec = todSec tod * 1000
     in ceiling sec == (floor sec :: Int)

class IsField a where
  fieldTag :: Proxy a -> Tag
  fieldIsData :: Proxy a -> Bool
  fieldToValue :: a -> ByteString
  fieldFromValue :: ByteString -> Either String a

-- Parses the value directly after the tag, including the '='.
--
-- The tag you pass in is for checking that the field was the right one
-- and for error messages.
fieldP :: forall a. (IsField a) => Tag -> Parsec Void ByteString a
fieldP tag = do
  let p = Proxy :: Proxy a
  let ft = fieldTag p
  void $ char 61 -- '='
  value <-
    if fieldIsData p
      then do
        guard $ tag == pred ft
        len <- decimal
        void $ char 1 -- 'SOH'
        dataTag <- decimal
        guard $ dataTag == ft
        void $ char 61 -- '='
        takeP (Just "octet") len
      else do
        guard $ tag == ft
        SB.pack <$> many (noneOf [1])
  void $ char 1 -- 'SOH'
  case fieldFromValue value of
    Left err -> fail err
    Right a -> pure a

fieldB :: forall a. (IsField a) => a -> ByteString.Builder
fieldB f =
  let p = Proxy :: Proxy a
      ft = fieldTag p
      value = fieldToValue f
   in mconcat $
        if fieldIsData p
          then
            [ BB.wordDec (pred ft),
              BB.char7 '=',
              BB.intDec $ SB.length value,
              BB.char7 '\SOH',
              BB.wordDec ft,
              BB.char7 '=',
              BB.byteString value,
              BB.char7 '\SOH'
            ]
          else
            [ BB.wordDec ft,
              BB.char7 '=',
              BB.byteString value,
              BB.char7 '\SOH'
            ]
