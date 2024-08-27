{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FIX.Core where

import Control.Arrow (left)
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder as ByteString
import qualified Data.ByteString.Lazy as LB
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Text ()
import Data.Validity.Time ()
import Data.Void
import Data.Word
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer
import Text.Read (readMaybe)

type Tag = Word

data Value
  = -- Simple value
    -- May not be empty
    -- May not contain SOH chars

    ValueSimple ByteString
  | -- Data value
    -- May not be empty
    -- May not contain SOH chars
    ValueData ByteString
  deriving (Show, Eq, Generic)

instance Validity Value where
  validate v =
    mconcat
      [ genericValidate v,
        case v of
          ValueSimple d ->
            mconcat
              [ declare "The value is nonempty" $ not (SB.null d),
                decorateList (SB.unpack d) $ \w ->
                  declare "The value is not '\\SOH'" $
                    w /= 1
              ]
          ValueData d ->
            declare "The value is nonempty" $ not (SB.null d)
      ]

valueByteString :: Value -> ByteString
valueByteString = \case
  ValueSimple bs -> bs
  ValueData bs -> bs

-- TODO use a boolean instead

data Field
  = Field
      -- If the value is ValueSimple, this is the field tag.
      -- If the value is ValueData, this is the length field tag.
      !Tag
      !Value
  deriving (Show, Eq, Generic)

instance Validity Field where
  validate f@(Field t v) =
    mconcat
      [ genericValidate f,
        case v of
          ValueSimple _ -> declare "The tag is not a length tag" $ not $ tagIsLen t
          ValueData _ -> declare "The tag is a length tag" $ tagIsLen t
      ]

newtype Message = Message
  { messageFields :: [Field]
  }
  deriving (Show, Eq, Generic)

instance Validity Message

parseMessage :: ByteString -> Either String Message
parseMessage = left errorBundlePretty . parse messageP "<pure>"

-- https://www.fixtrading.org/standards/tagvalue-online/
-- @
-- A well-formed field has the form:
-- tag=value<SOH>
--
-- A field shall be considered malformed if any of the following occurs as a result of encoding:
--
--     the tag is empty
--     the tag delimiter is missing
--     the value is empty
--     the value contains an <SOH> character and the datatype of the field is not data or XMLdata
--     the datatype of the field is data and the field is not immediately preceded by its associated Length field.

messageP :: Parsec Void ByteString Message
messageP = Message <$> many fieldP
  where
    fieldP = do
      tag <- decimal
      void $ char 61 -- '='
      if tagIsLen tag
        then do
          len <- decimal
          void $ char 1 -- 'SOH'
          dataTag <- decimal
          guard $ dataTag == succ tag
          void $ char 61 -- '='
          value <- takeP (Just "octet") len
          pure $ Field tag $ ValueData value
        else do
          value <- SB.pack <$> many (noneOf [1])
          void $ char 1 -- 'SOH'
          pure $ Field tag $ ValueSimple value

-- TODO Generate this?
tagIsLen :: Tag -> Bool
tagIsLen = \case
  90 -> True
  96 -> True
  360 -> True
  358 -> True
  348 -> True
  618 -> True
  352 -> True
  445 -> True
  350 -> True
  356 -> True
  354 -> True
  362 -> True
  364 -> True
  _ -> False

renderMessage :: Message -> ByteString
renderMessage = LB.toStrict . BB.toLazyByteString . buildMessage

buildMessage :: Message -> ByteString.Builder
buildMessage (Message fields) = flip foldMap fields $ \(Field w v) ->
  case v of
    ValueSimple bs ->
      mconcat
        [ BB.wordDec w,
          BB.char7 '=',
          BB.byteString bs,
          BB.char7 '\SOH'
        ]
    ValueData bs ->
      mconcat
        [ BB.wordDec w,
          BB.char7 '=',
          BB.intDec $ SB.length bs,
          BB.char7 '\SOH',
          BB.wordDec (succ w),
          BB.char7 '=',
          BB.byteString bs,
          BB.char7 '\SOH'
        ]

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

instance IsFieldType Word8 where
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
