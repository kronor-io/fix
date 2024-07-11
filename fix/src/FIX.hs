{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FIX where

import Control.Arrow (left)
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder as ByteString
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
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
import Text.Read

type Tag = Word

type Value = ByteString

data Field
  = FieldSimple
      -- Tag of the field
      Tag
      -- Value, may not contain SOH chars.
      Value
  | FieldData
      -- Tag of the length field
      -- Tag of the value field will be one more
      Tag
      -- Value, may contain SOH chars.
      ByteString
  deriving (Show, Eq, Generic)

instance Validity Field where
  validate f =
    mconcat
      [ genericValidate f,
        case f of
          FieldSimple _ v ->
            concat
              [ declare "The value is nonempty" $ not (SB.null v),
                decorateList (SB.unpack value) $ \w ->
                  declare "The value is not '\\SOH'" $
                    w /= 1
              ]
          FieldData _ d ->
            declare "The value is nonempty" $ not (SB.null v)
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
      value <- SB.pack <$> many (noneOf [1])
      void $ char 1 -- 'SOH'
      pure (tag, value)

renderMessage :: Message -> ByteString
renderMessage = LB.toStrict . BB.toLazyByteString . buildMessage

buildMessage :: Message -> ByteString.Builder
buildMessage (Message fields) = flip foldMap fields $ \(w, bs) ->
  mconcat
    [ BB.wordDec w,
      BB.char7 '=',
      BB.byteString bs,
      BB.char7 '\SOH'
    ]

class IsFieldType a where
  toValue :: a -> Value
  fromValue :: Value -> Maybe a

instance IsFieldType ByteString where
  toValue = id
  fromValue = Just

validateByteStringValue :: ByteString -> Validation
validateByteStringValue value =
  mconcat
    [ declare "The value is nonempty" $
        not $
          SB.null value,
      decorateList (SB.unpack value) $ \w ->
        declare "The value is not '\\SOH'" $
          w /= 1
    ]

instance IsFieldType Word where
  toValue = TE.encodeUtf8 . T.pack . show
  fromValue = readMaybe . T.unpack . TE.decodeLatin1

instance IsFieldType Int where
  toValue = TE.encodeUtf8 . T.pack . show
  fromValue = readMaybe . T.unpack . TE.decodeLatin1

instance IsFieldType UTCTime where
  toValue = TE.encodeUtf8 . T.pack . formatTime defaultTimeLocale utcTimeFormat
  fromValue = parseTimeM False defaultTimeLocale utcTimeFormat . T.unpack . TE.decodeLatin1

utcTimeFormat :: String
utcTimeFormat = "%Y%m%d-%X"

mkImpreciseUTCTime :: UTCTime -> UTCTime
mkImpreciseUTCTime u = u {utctDayTime = fromIntegral (floor (utctDayTime u) :: Word)}

validateImpreciseUTCTime :: UTCTime -> Validation
validateImpreciseUTCTime = validateImpreciseLocalTime . utcToLocalTime utc

validateImpreciseLocalTime :: LocalTime -> Validation
validateImpreciseLocalTime lt =
  let tod = localTimeOfDay lt
   in validateImpreciseTimeOfDay tod

validateImpreciseTimeOfDay :: TimeOfDay -> Validation
validateImpreciseTimeOfDay tod =
  declare "The number of seconds is integer" $
    let sec = todSec tod
     in ceiling sec == (floor sec :: Int)

class IsField a where
  fieldTag :: Proxy a -> Tag
  fieldToValue :: a -> Value
  fieldFromValue :: Value -> Maybe a

newtype BeginString = BeginString {unBeginString :: ByteString}
  deriving (Show, Eq, Generic)

instance Validity BeginString where
  validate trid@BeginString {..} =
    mconcat
      [ genericValidate trid,
        validateByteStringValue unBeginString
      ]

instance IsField BeginString where
  fieldTag Proxy = 8
  fieldToValue = unBeginString
  fieldFromValue = constructValid . BeginString

newtype BodyLength = BodyLength {unBodyLength :: Word}
  deriving (Show, Eq, Generic)

instance Validity BodyLength where
  validate trid@BodyLength {..} =
    mconcat
      [ genericValidate trid,
        declare "The body length is less than 10k" $ unBodyLength < 10000
      ]

instance IsField BodyLength where
  fieldTag Proxy = 9
  fieldToValue = toValue . unBodyLength
  fieldFromValue = fromValue >=> constructValid . BodyLength

newtype CheckSum = CheckSum {unCheckSum :: ByteString}
  deriving (Show, Eq, Generic)

instance Validity CheckSum where
  validate trid@CheckSum {..} =
    mconcat
      [ genericValidate trid,
        validateByteStringValue unCheckSum,
        declare "The checksum is exactly bytes long" $
          SB.length unCheckSum == 3
      ]

instance IsField CheckSum where
  fieldTag Proxy = 10
  fieldToValue = unCheckSum
  fieldFromValue = constructValid . CheckSum

newtype MessageSequenceNumber = MessageSequenceNumber {unMessageSequenceNumber :: Word}
  deriving (Show, Eq, Generic)

instance Validity MessageSequenceNumber where
  validate trid@MessageSequenceNumber {..} =
    mconcat
      [ genericValidate trid,
        declare "The body length is less than 100k" $ unMessageSequenceNumber < 100000
      ]

instance IsField MessageSequenceNumber where
  fieldTag Proxy = 34
  fieldToValue = toValue . unMessageSequenceNumber
  fieldFromValue = fromValue >=> constructValid . MessageSequenceNumber

data MessageType
  = MessageTypeHeartbeat
  | MessageTypeTestRequest
  | MessageTypeResendRequest
  | MessageTypeReject
  | MessageTypeSequenceReset
  | MessageTypeLogout
  | MessageTypeIndicationofInterest
  | MessageTypeAdvertisement
  | MessageTypeExecutionReport
  | MessageTypeOrderCancelReject
  | MessageTypeLogon
  | MessageTypeNews
  | MessageTypeEmail
  | MessageTypeNewOrderSingle
  | MessageTypeNewOrderList
  | MessageTypeOrderCancelRequest
  | MessageTypeOrderCancelReplaceRequest
  | MessageTypeOrderStatusRequest
  | MessageTypeAllocation
  | MessageTypeListCancelRequest
  | MessageTypeListExecute
  | MessageTypeListStatusRequest
  | MessageTypeListStatus
  | MessageTypeAllocationACK
  | MessageTypeDontKnowTrade
  | MessageTypeQuoteRequest
  | MessageTypeQuote
  deriving (Show, Eq, Generic)

instance Validity MessageType

instance IsField MessageType where
  fieldTag Proxy = 35
  fieldFromValue = \case
    "0" -> Just MessageTypeHeartbeat
    "1" -> Just MessageTypeTestRequest
    "2" -> Just MessageTypeResendRequest
    "3" -> Just MessageTypeReject
    "4" -> Just MessageTypeSequenceReset
    "5" -> Just MessageTypeLogout
    "6" -> Just MessageTypeIndicationofInterest
    "7" -> Just MessageTypeAdvertisement
    "8" -> Just MessageTypeExecutionReport
    "9" -> Just MessageTypeOrderCancelReject
    "A" -> Just MessageTypeLogon
    "B" -> Just MessageTypeNews
    "C" -> Just MessageTypeEmail
    "D" -> Just MessageTypeNewOrderSingle
    "E" -> Just MessageTypeNewOrderList
    "F" -> Just MessageTypeOrderCancelRequest
    "G" -> Just MessageTypeOrderCancelReplaceRequest
    "H" -> Just MessageTypeOrderStatusRequest
    "J" -> Just MessageTypeAllocation
    "K" -> Just MessageTypeListCancelRequest
    "L" -> Just MessageTypeListExecute
    "M" -> Just MessageTypeListStatusRequest
    "N" -> Just MessageTypeListStatus
    "P" -> Just MessageTypeAllocationACK
    "Q" -> Just MessageTypeDontKnowTrade
    "R" -> Just MessageTypeQuoteRequest
    "S" -> Just MessageTypeQuote
    _ -> Nothing
  fieldToValue = \case
    MessageTypeHeartbeat -> "0"
    MessageTypeTestRequest -> "1"
    MessageTypeResendRequest -> "2"
    MessageTypeReject -> "3"
    MessageTypeSequenceReset -> "4"
    MessageTypeLogout -> "5"
    MessageTypeIndicationofInterest -> "6"
    MessageTypeAdvertisement -> "7"
    MessageTypeExecutionReport -> "8"
    MessageTypeOrderCancelReject -> "9"
    MessageTypeLogon -> "A"
    MessageTypeNews -> "B"
    MessageTypeEmail -> "C"
    MessageTypeNewOrderSingle -> "D"
    MessageTypeNewOrderList -> "E"
    MessageTypeOrderCancelRequest -> "F"
    MessageTypeOrderCancelReplaceRequest -> "G"
    MessageTypeOrderStatusRequest -> "H"
    MessageTypeAllocation -> "J"
    MessageTypeListCancelRequest -> "K"
    MessageTypeListExecute -> "L"
    MessageTypeListStatusRequest -> "M"
    MessageTypeListStatus -> "N"
    MessageTypeAllocationACK -> "P"
    MessageTypeDontKnowTrade -> "Q"
    MessageTypeQuoteRequest -> "R"
    MessageTypeQuote -> "S"

newtype SenderCompId = SenderCompId {unSenderCompId :: ByteString}
  deriving (Show, Eq, Generic)

instance Validity SenderCompId where
  validate trid@SenderCompId {..} =
    mconcat
      [ genericValidate trid,
        validateByteStringValue unSenderCompId
      ]

instance IsField SenderCompId where
  fieldTag Proxy = 49
  fieldToValue = unSenderCompId
  fieldFromValue = constructValid . SenderCompId

newtype TargetCompId = TargetCompId {unTargetCompId :: ByteString}
  deriving (Show, Eq, Generic)

instance Validity TargetCompId where
  validate trid@TargetCompId {..} =
    mconcat
      [ genericValidate trid,
        validateByteStringValue unTargetCompId
      ]

newtype SendingTime = SendingTime {unSendingTime :: UTCTime}
  deriving (Show, Eq, Generic)

instance Validity SendingTime where
  validate st@SendingTime {..} =
    mconcat
      [ genericValidate st,
        validateImpreciseUTCTime unSendingTime
      ]

instance IsField SendingTime where
  fieldTag Proxy = 52
  fieldToValue = toValue . unSendingTime
  fieldFromValue = fromValue >=> constructValid . SendingTime

instance IsField TargetCompId where
  fieldTag Proxy = 56
  fieldToValue = unTargetCompId
  fieldFromValue = constructValid . TargetCompId

data EncryptionMethod
  = EncryptionMethodNoneOther
  | EncryptionMethodPKCS
  | EncryptionMethodDESECB
  | EncryptionMethodPKCSDES
  | EncryptionMethodPGPDES
  | EncryptionMethodPGPDESMD5
  | EncryptionMethodPEMDESMD5
  deriving (Show, Eq, Generic)

instance Validity EncryptionMethod

instance IsField EncryptionMethod where
  fieldTag Proxy = 98
  fieldToValue = \case
    EncryptionMethodNoneOther -> "0"
    EncryptionMethodPKCS -> "1"
    EncryptionMethodDESECB -> "2"
    EncryptionMethodPKCSDES -> "3"
    EncryptionMethodPGPDES -> "4"
    EncryptionMethodPGPDESMD5 -> "5"
    EncryptionMethodPEMDESMD5 -> "6"
  fieldFromValue = \case
    "0" -> Just EncryptionMethodNoneOther
    "1" -> Just EncryptionMethodPKCS
    "2" -> Just EncryptionMethodDESECB
    "3" -> Just EncryptionMethodPKCSDES
    "4" -> Just EncryptionMethodPGPDES
    "5" -> Just EncryptionMethodPGPDESMD5
    "6" -> Just EncryptionMethodPEMDESMD5
    _ -> Nothing

newtype HeartbeatInterval = HeartbeatInterval {unHeartbeatInterval :: Int}
  deriving (Show, Eq, Generic)

instance Validity HeartbeatInterval

instance IsField HeartbeatInterval where
  fieldTag Proxy = 108
  fieldToValue = TE.encodeUtf8 . T.pack . show . unHeartbeatInterval
  fieldFromValue = fmap HeartbeatInterval . readMaybe . T.unpack . TE.decodeLatin1

newtype TestRequestId = TestRequestId
  { unTestRequestId :: ByteString
  }
  deriving (Show, Eq, Generic)

instance Validity TestRequestId where
  validate trid@TestRequestId {..} =
    mconcat
      [ genericValidate trid,
        validateByteStringValue unTestRequestId
      ]

instance IsField TestRequestId where
  fieldTag Proxy = 112
  fieldToValue = unTestRequestId
  fieldFromValue = constructValid . TestRequestId

class IsMessage a where
  messageType :: Proxy a -> ByteString
  toMessageFields :: a -> [(Tag, ByteString)]
  fromMessage :: Message -> Maybe a

toMessage :: forall a. (IsMessage a) => a -> Message
toMessage =
  Message
    . ((35, messageType (Proxy :: Proxy a)) :)
    . toMessageFields

requiredFieldB :: forall a. (IsField a) => a -> Maybe (Tag, Value)
requiredFieldB a = Just (fieldTag (Proxy :: Proxy a), fieldToValue a)

optionalFieldB :: (IsField a) => Maybe a -> Maybe (Tag, Value)
optionalFieldB = (>>= requiredFieldB)

requiredFieldP :: forall a. (IsField a) => [(Tag, Value)] -> Maybe a
requiredFieldP fields = lookup (fieldTag (Proxy :: Proxy a)) fields >>= fieldFromValue

optionalFieldP :: forall a. (IsField a) => [(Tag, Value)] -> Maybe (Maybe a)
optionalFieldP fields = optional $ requiredFieldP fields

data MessageHeader = MessageHeader
  { messageHeaderBeginString :: !BeginString,
    messageHeaderBodyLength :: !BodyLength,
    messageHeaderMessageType :: !MessageType,
    messageHeaderSender :: !SenderCompId,
    messageHeaderTarget :: !TargetCompId,
    messageHeaderMessageSequenceNumber :: !MessageSequenceNumber
  }

parseMessageHeader :: [(Tag, Value)] -> Maybe MessageHeader
parseMessageHeader fields = do
  messageHeaderBeginString <- requiredFieldP fields
  messageHeaderBodyLength <- requiredFieldP fields
  messageHeaderMessageType <- requiredFieldP fields
  messageHeaderSender <- requiredFieldP fields
  messageHeaderTarget <- requiredFieldP fields
  messageHeaderMessageSequenceNumber <- requiredFieldP fields
  pure MessageHeader {..}

data MessageTrailer = MessageTrailer
  { messageTrailerCheckSum :: !CheckSum
  }

parseMessageTrailer :: [(Tag, Value)] -> Maybe MessageTrailer
parseMessageTrailer fields = do
  messageTrailerCheckSum <- requiredFieldP fields
  pure MessageTrailer {..}

data LogonMessage = LogonMessage
  { logonMessageEncryptMethod :: !EncryptionMethod,
    logonMessageHeartBeatInterval :: !HeartbeatInterval
  }
  deriving (Show, Eq, Generic)

instance Validity LogonMessage

instance IsMessage LogonMessage where
  messageType Proxy = "A"
  toMessageFields LogonMessage {..} =
    catMaybes
      [ requiredFieldB logonMessageEncryptMethod,
        requiredFieldB logonMessageHeartBeatInterval
      ]
  fromMessage (Message fields) = do
    logonMessageEncryptMethod <- requiredFieldP fields
    logonMessageHeartBeatInterval <- requiredFieldP fields
    pure LogonMessage {..}

data HeartbeatMessage = HeartbeatMessage
  { heartbeatMessageTestRequestId :: !(Maybe TestRequestId)
  }
  deriving (Show, Eq, Generic)

instance Validity HeartbeatMessage

instance IsMessage HeartbeatMessage where
  messageType Proxy = "0"
  toMessageFields HeartbeatMessage {..} =
    catMaybes
      [ optionalFieldB heartbeatMessageTestRequestId
      ]
  fromMessage (Message fields) = do
    heartbeatMessageTestRequestId <- optionalFieldP fields
    pure HeartbeatMessage {..}
