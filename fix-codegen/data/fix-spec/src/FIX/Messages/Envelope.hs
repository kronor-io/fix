{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FIX.Messages.Envelope where

import Control.Monad
import Control.Monad.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LB
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Validity
import Data.Word
import FIX.Components.Class
import FIX.Core
import FIX.Fields
import FIX.Messages.Class
import FIX.Messages.Header
import FIX.Messages.Trailer
import GHC.Generics (Generic)
import Text.Printf

data Envelope a = Envelope
  { envelopeHeader :: Header,
    envelopeContents :: a,
    envelopeTrailer :: Trailer
  }
  deriving (Show, Eq, Generic)

instance (Validity a) => Validity (Envelope a)

renderHeader :: Header -> [AnyField]
renderHeader = toComponentFields

renderTrailer :: Trailer -> [AnyField]
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
   in mconcat
        -- TODO figure out what to do about the message type being in the
        -- header already
        [ renderHeader (envelopeHeader e),
          toComponentFields (envelopeContents e),
          renderTrailer (envelopeTrailer e)
        ]

parseAnyFields :: ByteString -> Either String [AnyField]
parseAnyFields = undefined

renderAnyFields :: [AnyField] -> ByteString
renderAnyFields = LB.toStrict . Builder.toLazyByteString . foldMap anyFieldB

-- Has to happen _before_ fixEnvelopeCheckSum
fixEnvelopeBodyLength :: (IsMessage a) => Envelope a -> Envelope a
fixEnvelopeBodyLength e =
  let bodyLength = computeBodyLength e
   in e {envelopeHeader = (envelopeHeader e) {headerBodyLength = bodyLength}}

computeBodyLength :: (IsMessage a) => Envelope a -> BodyLength
computeBodyLength Envelope {..} =
  let bytesBeforeBodyLength =
        computeFieldsLength
          [ packAnyField $ headerBeginString envelopeHeader,
            packAnyField $ headerBodyLength envelopeHeader
          ]
      allFields =
        concat
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
  (IsMessage a) =>
  Envelope a ->
  Envelope a
fixEnvelopeCheckSum e@Envelope {..} =
  let fieldsUntilCheckSum =
        concat
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
