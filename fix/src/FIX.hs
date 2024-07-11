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
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Text ()
import Data.Void
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer

type Tag = Word

newtype Message = Message
  { messageFields :: [(Tag, ByteString)]
  }
  deriving (Show, Eq, Generic)

instance Validity Message where
  validate m@(Message fields) =
    mconcat
      [ genericValidate m,
        decorateList fields $ \(_, value) ->
          mconcat
            [ declare "The value is nonempty" $ not (SB.null value),
              decorateList (SB.unpack value) $ \w ->
                declare "The value is not '\\SOH'" $
                  w /= 1
            ]
      ]

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

class IsMessage a where
  messageType :: Proxy a -> ByteString
  toMessageFields :: a -> [(Tag, ByteString)]
  fromMessage :: Message -> Maybe a

toMessage :: forall a. (IsMessage a) => a -> Message
toMessage =
  Message
    . ((35, messageType (Proxy :: Proxy a)) :)
    . toMessageFields

class IsField a where
  fieldTag :: Proxy a -> Tag
  toValue :: a -> ByteString
  fromValue :: ByteString -> Maybe a

newtype TestRequestId = TestRequestId
  { unTestRequestId :: ByteString
  }
  deriving (Show, Eq, Generic)

instance Validity TestRequestId where
  validate trid@TestRequestId {..} =
    mconcat
      [ genericValidate trid,
        declare "The value is nonempty" $
          not $
            SB.null unTestRequestId,
        decorateList (SB.unpack unTestRequestId) $ \w ->
          declare "The value is not '\\SOH'" $
            w /= 1
      ]

instance IsField TestRequestId where
  fieldTag Proxy = 112
  toValue = unTestRequestId
  fromValue = constructValid . TestRequestId

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
  toValue = \case
    EncryptionMethodNoneOther -> "0"
    EncryptionMethodPKCS -> "1"
    EncryptionMethodDESECB -> "2"
    EncryptionMethodPKCSDES -> "3"
    EncryptionMethodPGPDES -> "4"
    EncryptionMethodPGPDESMD5 -> "5"
    EncryptionMethodPEMDESMD5 -> "6"
  fromValue = \case
    "0" -> Just EncryptionMethodNoneOther
    "1" -> Just EncryptionMethodPKCS
    "2" -> Just EncryptionMethodDESECB
    "3" -> Just EncryptionMethodPKCSDES
    "4" -> Just EncryptionMethodPGPDES
    "5" -> Just EncryptionMethodPGPDESMD5
    "6" -> Just EncryptionMethodPEMDESMD5
    _ -> Nothing

data LogonMessage = LogonMessage
  { logonMessageEncryptMethod :: !EncryptionMethod,
    logonMessageHeartBeatInterval :: !ByteString
  }
  deriving (Show, Eq, Generic)

instance Validity LogonMessage where
  validate lm@LogonMessage {..} =
    mconcat
      [ genericValidate lm,
        declare "The heartbeat interval is nonempty" $ not $ SB.null logonMessageHeartBeatInterval,
        decorate "The heartbeat interval has no '\\SOH' characters" $
          decorateList (SB.unpack logonMessageHeartBeatInterval) $ \w ->
            declare "The value is not '\\SOH'" $
              w /= 1
      ]

instance IsMessage LogonMessage where
  messageType Proxy = "A"
  toMessageFields LogonMessage {..} =
    [ (98, toValue logonMessageEncryptMethod),
      (108, logonMessageHeartBeatInterval)
    ]
  fromMessage (Message fields) = do
    logonMessageEncryptMethod <- lookup 98 fields >>= fromValue
    logonMessageHeartBeatInterval <- lookup 108 fields
    pure LogonMessage {..}

data HeartbeatMessage = HeartbeatMessage
  { heartbeatMessageTestRequestId :: Maybe TestRequestId
  }
  deriving (Show, Eq, Generic)

instance Validity HeartbeatMessage

instance IsMessage HeartbeatMessage where
  messageType Proxy = "0"
  toMessageFields HeartbeatMessage {..} =
    [(112, toValue testRequestId) | testRequestId <- maybeToList heartbeatMessageTestRequestId]
  fromMessage (Message fields) = do
    heartbeatMessageTestRequestId <- optional $ lookup 112 fields >>= fromValue
    pure HeartbeatMessage {..}
