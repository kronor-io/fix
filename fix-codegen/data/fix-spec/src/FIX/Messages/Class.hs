{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FIX.Messages.Class where

import Control.Arrow (second)
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString as SB
import Data.Maybe
import Data.Proxy
import Data.Validity
import FIX.Core
import FIX.Fields.BeginString
import FIX.Fields.BodyLength
import FIX.Fields.CheckSum
import FIX.Fields.EncryptMethod
import FIX.Fields.HeartBtInt
import FIX.Fields.MsgSeqNum
import FIX.Fields.MsgType
import FIX.Fields.Password
import FIX.Fields.SenderCompID
import FIX.Fields.SenderSubID
import FIX.Fields.SendingTime
import FIX.Fields.TargetCompID
import GHC.Generics (Generic)

type MessageP a = StateT [Field] (Except MessageParseError) a

data MessageParseError
  = MessageParseErrorMessageTypeMismatch !MsgType !MsgType
  | MessageParseErrorMissingField !Tag
  | MessageParseErrorFieldParseError !Tag !String
  deriving (Show)

class IsMessage a where
  messageType :: Proxy a -> MsgType
  toMessageFields :: a -> [Field]
  fromMessageFields :: MessageP a

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
    messageHeaderMsgType :: !MsgType,
    messageHeaderSender :: !SenderCompID,
    messageHeaderSenderSubId :: !(Maybe SenderSubID),
    messageHeaderTarget :: !TargetCompID,
    messageHeaderMessageSequenceNumber :: !MsgSeqNum,
    messageHeaderSendingTime :: !SendingTime
  }
  deriving (Show, Eq, Generic)

instance Validity MessageHeader

parseMessageHeader :: MessageP MessageHeader
parseMessageHeader = do
  messageHeaderBeginString <- requiredFieldP
  messageHeaderBodyLength <- requiredFieldP
  messageHeaderMsgType <- requiredFieldP
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
      requiredFieldB messageHeaderMsgType,
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

data Envelope a = Envelope
  { envelopeHeader :: MessageHeader,
    envelopeContents :: a,
    envelopeTrailer :: MessageTrailer
  }
  deriving (Show, Eq, Generic)

instance (Validity a) => Validity (Envelope a)

fromMessage ::
  forall a.
  (IsMessage a) =>
  Message ->
  Either MessageParseError (Envelope a)
fromMessage message = runExcept $ flip evalStateT (messageFields message) $ do
  -- TODO consider erroring on unexpected fields?
  envelopeHeader <- parseMessageHeader
  let actualTag = messageHeaderMsgType envelopeHeader
  let expectedTag = messageType (Proxy :: Proxy a)
  when (actualTag /= expectedTag) $ throwError $ MessageParseErrorMessageTypeMismatch actualTag expectedTag
  envelopeContents <- fromMessageFields
  envelopeTrailer <- parseMessageTrailer
  pure Envelope {..}

toMessage :: forall a. (IsMessage a) => Envelope a -> Message
toMessage e'' =
  let h = (envelopeHeader e'') {messageHeaderMsgType = messageType (Proxy :: Proxy a)}
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

fixEnvelopeCheckSum ::
  (IsMessage a) =>
  Envelope a ->
  Envelope a
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
