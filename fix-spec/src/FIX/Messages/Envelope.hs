{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages.Envelope where

import Control.Arrow (left)
import Control.Monad
import Control.Monad.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder as ByteString
import qualified Data.ByteString.Lazy as LB
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Validity
import Data.Void
import Data.Word
import FIX.Components.Class
import FIX.Core
import FIX.Fields
import FIX.Messages.Class
import FIX.Messages.Header
import FIX.Messages.Trailer
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Printf

data Envelope a = Envelope
  { envelopeHeader :: Header,
    envelopeContents :: a,
    envelopeTrailer :: Trailer
  }
  deriving (Show, Eq, Generic, Functor)

instance (Validity a) => Validity (Envelope a)

renderHeader :: Header -> DList AnyField
renderHeader = toComponentFields

renderTrailer :: Trailer -> DList AnyField
renderTrailer = toComponentFields

parseHeader :: ComponentP Header
parseHeader = fromComponentFields

parseTrailer :: ComponentP Trailer
parseTrailer = fromComponentFields

fromFields ::
  forall a.
  (IsMessage a) =>
  [AnyField] ->
  Either ComponentParseError (Envelope a)
fromFields fields = runComponentP fields $ do
  -- TODO consider erroring on unexpected fields?
  envelopeHeader <- parseHeader
  let actualTag = headerMsgType envelopeHeader
  let expectedTag = messageType (Proxy :: Proxy a)
  when (actualTag /= expectedTag) $ throwError $ ComponentParseErrorMsgTypeMismatch actualTag expectedTag
  envelopeContents <- fromComponentFields
  envelopeTrailer <- parseTrailer
  pure Envelope {..}

toFields :: forall a. (IsMessage a) => Envelope a -> [AnyField]
toFields e'' =
  let h = (envelopeHeader e'') {headerMsgType = messageType (Proxy :: Proxy a)}
      e' = e'' {envelopeHeader = h}
      e = fixEnvelopeCheckSum $ fixEnvelopeBodyLength e'
   in DList.toList $
        mconcat
          -- TODO figure out what to do about the message type being in the
          -- header already
          [ renderHeader (envelopeHeader e),
            toComponentFields (envelopeContents e),
            renderTrailer (envelopeTrailer e)
          ]

parseAnyFields :: ByteString -> Either String [AnyField]
parseAnyFields = left errorBundlePretty . parse (many anyFieldP) "<pure>"

renderAnyFields :: [AnyField] -> ByteString
renderAnyFields = LB.toStrict . Builder.toLazyByteString . foldMap anyFieldB

-- Has to happen _before_ fixEnvelopeCheckSum
fixEnvelopeBodyLength :: (IsComponent a) => Envelope a -> Envelope a
fixEnvelopeBodyLength e =
  let bodyLength = computeBodyLength e
   in e {envelopeHeader = (envelopeHeader e) {headerBodyLength = bodyLength}}

computeBodyLength :: (IsComponent a) => Envelope a -> BodyLength
computeBodyLength Envelope {..} =
  let bytesBeforeBodyLength =
        computeFieldsLength
          [ packAnyField $ headerBeginString envelopeHeader,
            packAnyField $ headerBodyLength envelopeHeader
          ]
      allFields =
        DList.toList $
          mconcat
            [ renderHeader envelopeHeader,
              toComponentFields envelopeContents,
              renderTrailer envelopeTrailer
            ]
      bytesFromCheckSum =
        computeFieldsLength
          [ packAnyField $ trailerCheckSum envelopeTrailer
          ]
   in BodyLength $
        computeFieldsLength allFields
          - bytesBeforeBodyLength
          - bytesFromCheckSum

computeFieldsLength :: [AnyField] -> Word
computeFieldsLength fields =
  let bytes = renderAnyFields fields
   in fromIntegral $ SB.length bytes

fixEnvelopeCheckSum ::
  (IsComponent a) =>
  Envelope a ->
  Envelope a
fixEnvelopeCheckSum e@Envelope {..} =
  let fieldsUntilCheckSum =
        DList.toList $
          mconcat
            -- TODO figure out what to do about the message type being in the
            -- header already
            [ renderHeader envelopeHeader,
              toComponentFields envelopeContents
            ]
      checkSum = computeCheckSum fieldsUntilCheckSum
   in e {envelopeTrailer = envelopeTrailer {trailerCheckSum = checkSum}}

computeCheckSum :: [AnyField] -> CheckSum
computeCheckSum fields =
  let bytes = renderAnyFields fields
      w = sum $ SB.unpack bytes
   in renderCheckSum w

renderCheckSum :: Word8 -> CheckSum
renderCheckSum = CheckSum . SimpleBytes . TE.encodeUtf8 . T.pack . printf "%03d"

incrementMsgSeqNum :: MsgSeqNum -> MsgSeqNum
incrementMsgSeqNum (MsgSeqNum w) = MsgSeqNum (succ w)

messageB :: (IsMessage a) => Header -> Trailer -> a -> ByteString.Builder
messageB envelopeHeader envelopeTrailer envelopeContents =
  foldMap anyFieldB $ toFields $ fixEnvelopeCheckSum $ fixEnvelopeBodyLength Envelope {..}

messageP :: (IsMessage a) => BeginString -> BodyLength -> MsgType -> Parsec Void ByteString (Envelope a)
messageP bs bl@(BodyLength l) typ = do
  let msgTypeBytes = computeFieldsLength [packAnyField typ]
      bytesFromCheckSum =
        computeFieldsLength
          [ packAnyField $ renderCheckSum 0
          ]
  let bytesToRead = l - msgTypeBytes + bytesFromCheckSum
  bytes <- takeP (Just "octet") (fromIntegral bytesToRead)
  restFields <- case parseAnyFields bytes of
    Left err -> fail err
    Right fields -> pure fields
  let allFields = [SomeBeginString bs, SomeBodyLength bl, SomeMsgType typ] ++ restFields
  case fromFields allFields of
    Left err -> fail $ show err
    Right e -> do
      -- TODO check the checksum
      pure e
