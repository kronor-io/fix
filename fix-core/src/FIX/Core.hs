{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FIX.Core where

import Control.Arrow (left, second)
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
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
import Data.Word
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer
import Text.Printf
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

data Envelope a = Envelope
  { envelopeHeader :: MessageHeader,
    envelopeContents :: a,
    envelopeTrailer :: MessageTrailer
  }
  deriving (Show, Eq, Generic)

instance (Validity a) => Validity (Envelope a)

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

instance IsFieldType UTCTime where
  toValue = TE.encodeUtf8 . T.pack . formatTime defaultTimeLocale "%Y%m%d-%H:%M:%S%03Q"
  fromValue sb =
    let s = T.unpack $ TE.decodeLatin1 sb
     in case parseTimeM False defaultTimeLocale utcTimeFormatExact s
          <|> parseTimeM False defaultTimeLocale utcTimeFormat s of
          Nothing -> Left $ "Could not Read UTCTime from String: " <> show s
          Just u -> Right u

utcTimeFormat :: String
utcTimeFormat = "%Y%m%d-%X"

utcTimeFormatExact :: String
utcTimeFormatExact = "%Y%m%d-%H:%M:%S%Q"

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
  declare "The number of seconds has at most three digits" $
    let sec = todSec tod * 1000
     in ceiling sec == (floor sec :: Int)

class IsField a where
  fieldTag :: Proxy a -> Tag
  fieldIsData :: Proxy a -> Bool
  fieldToValue :: a -> ByteString
  fieldFromValue :: ByteString -> Either String a

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
  fieldIsData Proxy = False
  fieldToValue = unBeginString
  fieldFromValue = prettyValidate . BeginString

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
  fieldIsData Proxy = False
  fieldToValue = toValue . unBodyLength
  fieldFromValue = fromValue >=> prettyValidate . BodyLength

newtype CheckSum = CheckSum {unCheckSum :: Word8}
  deriving (Show, Eq, Generic)

instance Validity CheckSum

instance IsField CheckSum where
  fieldTag Proxy = 10
  fieldIsData Proxy = False
  fieldToValue = TE.encodeUtf8 . T.pack . printf "%03d" . unCheckSum
  fieldFromValue = fromValue >=> prettyValidate . CheckSum

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
  fieldIsData Proxy = False
  fieldToValue = toValue . unMessageSequenceNumber
  fieldFromValue = fromValue >=> prettyValidate . MessageSequenceNumber

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
  fieldIsData Proxy = False
  fieldFromValue = \case
    "0" -> Right MessageTypeHeartbeat
    "1" -> Right MessageTypeTestRequest
    "2" -> Right MessageTypeResendRequest
    "3" -> Right MessageTypeReject
    "4" -> Right MessageTypeSequenceReset
    "5" -> Right MessageTypeLogout
    "6" -> Right MessageTypeIndicationofInterest
    "7" -> Right MessageTypeAdvertisement
    "8" -> Right MessageTypeExecutionReport
    "9" -> Right MessageTypeOrderCancelReject
    "A" -> Right MessageTypeLogon
    "B" -> Right MessageTypeNews
    "C" -> Right MessageTypeEmail
    "D" -> Right MessageTypeNewOrderSingle
    "E" -> Right MessageTypeNewOrderList
    "F" -> Right MessageTypeOrderCancelRequest
    "G" -> Right MessageTypeOrderCancelReplaceRequest
    "H" -> Right MessageTypeOrderStatusRequest
    "J" -> Right MessageTypeAllocation
    "K" -> Right MessageTypeListCancelRequest
    "L" -> Right MessageTypeListExecute
    "M" -> Right MessageTypeListStatusRequest
    "N" -> Right MessageTypeListStatus
    "P" -> Right MessageTypeAllocationACK
    "Q" -> Right MessageTypeDontKnowTrade
    "R" -> Right MessageTypeQuoteRequest
    "S" -> Right MessageTypeQuote
    s -> Left $ "Unknown MessageType: " <> show s
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
  fieldIsData Proxy = False
  fieldToValue = unSenderCompId
  fieldFromValue = prettyValidate . SenderCompId

newtype SenderSubId = SenderSubId {unSenderSubId :: ByteString}
  deriving (Show, Eq, Generic)

instance Validity SenderSubId where
  validate trid@SenderSubId {..} =
    mconcat
      [ genericValidate trid,
        validateByteStringValue unSenderSubId
      ]

instance IsField SenderSubId where
  fieldTag Proxy = 50
  fieldIsData Proxy = False
  fieldToValue = unSenderSubId
  fieldFromValue = prettyValidate . SenderSubId

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
  fieldIsData Proxy = False
  fieldToValue = toValue . unSendingTime
  fieldFromValue = fromValue >=> prettyValidate . SendingTime

instance IsField TargetCompId where
  fieldTag Proxy = 56
  fieldIsData Proxy = False
  fieldToValue = unTargetCompId
  fieldFromValue = prettyValidate . TargetCompId

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
  fieldIsData Proxy = False
  fieldToValue = \case
    EncryptionMethodNoneOther -> "0"
    EncryptionMethodPKCS -> "1"
    EncryptionMethodDESECB -> "2"
    EncryptionMethodPKCSDES -> "3"
    EncryptionMethodPGPDES -> "4"
    EncryptionMethodPGPDESMD5 -> "5"
    EncryptionMethodPEMDESMD5 -> "6"
  fieldFromValue = \case
    "0" -> Right EncryptionMethodNoneOther
    "1" -> Right EncryptionMethodPKCS
    "2" -> Right EncryptionMethodDESECB
    "3" -> Right EncryptionMethodPKCSDES
    "4" -> Right EncryptionMethodPGPDES
    "5" -> Right EncryptionMethodPGPDESMD5
    "6" -> Right EncryptionMethodPEMDESMD5
    s -> Left $ "Unknown EncryptionMethod: " <> show s

newtype HeartbeatInterval = HeartbeatInterval {unHeartbeatInterval :: Int}
  deriving (Show, Eq, Generic)

instance Validity HeartbeatInterval

instance IsField HeartbeatInterval where
  fieldTag Proxy = 108
  fieldIsData Proxy = False
  fieldToValue = toValue . unHeartbeatInterval
  fieldFromValue = fromValue >=> prettyValidate . HeartbeatInterval

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
  fieldIsData Proxy = False
  fieldToValue = unTestRequestId
  fieldFromValue = prettyValidate . TestRequestId

newtype ResetSequenceNumbersFlag = ResetSequenceNumbersFlag
  { unResetSequenceNumbersFlag :: Bool
  }
  deriving (Show, Eq, Generic)

instance Validity ResetSequenceNumbersFlag

instance IsField ResetSequenceNumbersFlag where
  fieldTag Proxy = 141
  fieldIsData Proxy = False
  fieldToValue = toValue . unResetSequenceNumbersFlag
  fieldFromValue = fromValue >=> prettyValidate . ResetSequenceNumbersFlag

newtype Password = Password
  { unPassword :: ByteString
  }
  deriving (Show, Eq, Generic)

instance Validity Password where
  validate trid@Password {..} =
    mconcat
      [ genericValidate trid,
        validateByteStringValue unPassword
      ]

instance IsField Password where
  fieldTag Proxy = 554
  fieldIsData Proxy = False
  fieldToValue = unPassword
  fieldFromValue = prettyValidate . Password

type MessageP a = StateT [Field] (Except MessageParseError) a

data MessageParseError
  = MessageParseErrorMessageTypeMismatch !MessageType !MessageType
  | MessageParseErrorMissingField !Tag
  | MessageParseErrorFieldParseError !Tag !String
  deriving (Show)

requiredFieldP :: forall a. (IsField a) => MessageP a
requiredFieldP = do
  let tag = fieldTag (Proxy :: Proxy a)
  mA <- optionalFieldP
  case mA of
    Nothing -> throwError $ MessageParseErrorMissingField tag
    Just a -> pure a

optionalFieldP :: forall a. (IsField a) => MessageP (Maybe a)
optionalFieldP = do
  fields <- get
  let tag = fieldTag (Proxy :: Proxy a)
  let go = \case
        [] -> Nothing
        (f@(Field t v) : fs) ->
          if t == tag
            then pure (v, fs)
            else second (f :) <$> go fs

  case go fields of
    Nothing -> pure Nothing
    Just (b, fields') -> case fieldFromValue (valueByteString b) of
      Left err -> throwError $ MessageParseErrorFieldParseError tag err
      Right v -> do
        put fields'
        pure (Just v)

class IsMessage a where
  messageType :: Proxy a -> MessageType
  toMessageFields :: a -> [Field]
  fromMessageFields :: MessageP a

toMessage :: forall a. (IsMessage a) => Envelope a -> Message
toMessage e'' =
  let h = (envelopeHeader e'') {messageHeaderMessageType = messageType (Proxy :: Proxy a)}
      e' = e'' {envelopeHeader = h}
      e = fixEnvelopeCheckSum $ fixEnvelopeBodyLength e'
   in Message
        { messageFields =
            concat
              -- TODO figure out what to do about the message type being in the
              -- header already
              [ renderMessageHeader (envelopeHeader e),
                toMessageFields (envelopeContents e),
                renderMessageTrailer (envelopeTrailer e)
              ]
        }

-- Has to happen _before_ fixEnvelopeCheckSum
fixEnvelopeBodyLength :: (IsMessage a) => Envelope a -> Envelope a
fixEnvelopeBodyLength e =
  let bodyLength = computeBodyLength e
   in e {envelopeHeader = (envelopeHeader e) {messageHeaderBodyLength = bodyLength}}

computeBodyLength :: (IsMessage a) => Envelope a -> BodyLength
computeBodyLength Envelope {..} =
  let bytesBeforeBodyLength =
        computeFieldsLength
          [ fieldB $ messageHeaderBeginString envelopeHeader,
            fieldB $ messageHeaderBodyLength envelopeHeader
          ]
      allFields =
        concat
          [ renderMessageHeader envelopeHeader,
            toMessageFields envelopeContents,
            renderMessageTrailer envelopeTrailer
          ]
      bytesFromCheckSum = computeFieldsLength [fieldB $ messageTrailerCheckSum envelopeTrailer]
   in BodyLength $
        computeFieldsLength allFields
          - bytesBeforeBodyLength
          - bytesFromCheckSum

computeFieldsLength :: [Field] -> Word
computeFieldsLength fields =
  let bytes = renderMessage (Message {messageFields = fields})
   in fromIntegral $ SB.length bytes

fixEnvelopeCheckSum :: (IsMessage a) => Envelope a -> Envelope a
fixEnvelopeCheckSum e@Envelope {..} =
  let fieldsUntilCheckSum =
        concat
          -- TODO figure out what to do about the message type being in the
          -- header already
          [ renderMessageHeader envelopeHeader,
            toMessageFields envelopeContents
          ]
      checkSum = computeCheckSum fieldsUntilCheckSum
   in e {envelopeTrailer = envelopeTrailer {messageTrailerCheckSum = checkSum}}

computeCheckSum :: [Field] -> CheckSum
computeCheckSum fields =
  let bytes = renderMessage (Message {messageFields = fields})
      w = sum $ SB.unpack bytes
   in CheckSum w

fromMessage ::
  forall a.
  (IsMessage a) =>
  Message ->
  Either MessageParseError (Envelope a)
fromMessage message = runExcept $ flip evalStateT (messageFields message) $ do
  -- TODO consider erroring on unexpected fields?
  envelopeHeader <- parseMessageHeader
  let actualTag = messageHeaderMessageType envelopeHeader
  let expectedTag = messageType (Proxy :: Proxy a)
  when (actualTag /= expectedTag) $ throwError $ MessageParseErrorMessageTypeMismatch actualTag expectedTag
  envelopeContents <- fromMessageFields
  envelopeTrailer <- parseMessageTrailer
  pure Envelope {..}

fieldB :: forall a. (IsField a) => a -> Field
fieldB a =
  let p = (Proxy :: Proxy a)
   in Field
        (fieldTag p)
        ( ( if fieldIsData p
              then ValueData
              else ValueSimple
          )
            (fieldToValue a)
        )

requiredFieldB :: (IsField a) => a -> Maybe Field
requiredFieldB = Just . fieldB

optionalFieldB :: (IsField a) => Maybe a -> Maybe Field
optionalFieldB = (>>= requiredFieldB)

data MessageHeader = MessageHeader
  { messageHeaderBeginString :: !BeginString,
    messageHeaderBodyLength :: !BodyLength,
    messageHeaderMessageType :: !MessageType,
    messageHeaderSender :: !SenderCompId,
    messageHeaderSenderSubId :: !(Maybe SenderSubId),
    messageHeaderTarget :: !TargetCompId,
    messageHeaderMessageSequenceNumber :: !MessageSequenceNumber,
    messageHeaderSendingTime :: !SendingTime
  }
  deriving (Show, Eq, Generic)

instance Validity MessageHeader

parseMessageHeader :: MessageP MessageHeader
parseMessageHeader = do
  messageHeaderBeginString <- requiredFieldP
  messageHeaderBodyLength <- requiredFieldP
  messageHeaderMessageType <- requiredFieldP
  messageHeaderSender <- requiredFieldP
  messageHeaderSenderSubId <- optionalFieldP
  messageHeaderTarget <- requiredFieldP
  messageHeaderMessageSequenceNumber <- requiredFieldP
  messageHeaderSendingTime <- requiredFieldP
  pure MessageHeader {..}

renderMessageHeader :: MessageHeader -> [Field]
renderMessageHeader MessageHeader {..} =
  catMaybes
    [ requiredFieldB messageHeaderBeginString,
      requiredFieldB messageHeaderBodyLength,
      requiredFieldB messageHeaderMessageType,
      requiredFieldB messageHeaderSender,
      optionalFieldB messageHeaderSenderSubId,
      requiredFieldB messageHeaderTarget,
      requiredFieldB messageHeaderMessageSequenceNumber,
      requiredFieldB messageHeaderSendingTime
    ]

data MessageTrailer = MessageTrailer
  { messageTrailerCheckSum :: !CheckSum
  }
  deriving (Show, Eq, Generic)

instance Validity MessageTrailer

parseMessageTrailer :: MessageP MessageTrailer
parseMessageTrailer = do
  messageTrailerCheckSum <- requiredFieldP
  pure MessageTrailer {..}

renderMessageTrailer :: MessageTrailer -> [Field]
renderMessageTrailer MessageTrailer {..} =
  catMaybes
    [ requiredFieldB messageTrailerCheckSum
    ]

data LogonMessage = LogonMessage
  { logonMessageEncryptMethod :: !EncryptionMethod,
    logonMessageHeartBeatInterval :: !HeartbeatInterval,
    logonMessageResetSequenceNumbersFlag :: !(Maybe ResetSequenceNumbersFlag),
    logonMessagePassword :: !(Maybe Password)
  }
  deriving (Show, Eq, Generic)

instance Validity LogonMessage

instance IsMessage LogonMessage where
  messageType Proxy = MessageTypeLogon
  toMessageFields LogonMessage {..} =
    catMaybes
      [ requiredFieldB logonMessageEncryptMethod,
        requiredFieldB logonMessageHeartBeatInterval,
        optionalFieldB logonMessageResetSequenceNumbersFlag,
        optionalFieldB logonMessagePassword
      ]
  fromMessageFields = do
    logonMessageEncryptMethod <- requiredFieldP
    logonMessageHeartBeatInterval <- requiredFieldP
    logonMessageResetSequenceNumbersFlag <- optionalFieldP
    logonMessagePassword <- optionalFieldP
    pure LogonMessage {..}

data HeartbeatMessage = HeartbeatMessage
  { heartbeatMessageTestRequestId :: !(Maybe TestRequestId)
  }
  deriving (Show, Eq, Generic)

instance Validity HeartbeatMessage

instance IsMessage HeartbeatMessage where
  messageType Proxy = MessageTypeHeartbeat
  toMessageFields HeartbeatMessage {..} =
    catMaybes
      [ optionalFieldB heartbeatMessageTestRequestId
      ]
  fromMessageFields = do
    heartbeatMessageTestRequestId <- optionalFieldP
    pure HeartbeatMessage {..}
