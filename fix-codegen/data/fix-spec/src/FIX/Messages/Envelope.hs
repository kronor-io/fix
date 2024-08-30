{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FIX.Messages.Envelope where

import Control.Monad
import Control.Monad.Except
import qualified Data.ByteString as SB
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Validity
import Data.Word
import FIX.Components.Class
import FIX.Core
import FIX.Fields.BodyLength
import FIX.Fields.CheckSum
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

renderHeader :: Header -> [Field]
renderHeader = toComponentFields

renderTrailer :: Trailer -> [Field]
renderTrailer = toComponentFields

parseHeader :: ComponentP Header
parseHeader = fromComponentFields

parseTrailer :: ComponentP Trailer
parseTrailer = fromComponentFields

fromMessage ::
  forall a.
  (IsMessage a) =>
  Message ->
  Either ComponentParseError (Envelope a)
fromMessage message = runComponentP (messageFields message) $ do
  -- TODO consider erroring on unexpected fields?
  envelopeHeader <- parseHeader
  let actualTag = headerMsgType envelopeHeader
  let expectedTag = messageType (Proxy :: Proxy a)
  when (actualTag /= expectedTag) $ throwError $ ComponentParseErrorMsgTypeMismatch actualTag expectedTag
  envelopeContents <- fromComponentFields
  envelopeTrailer <- parseTrailer
  pure Envelope {..}

toMessage :: forall a. (IsMessage a) => Envelope a -> Message
toMessage e'' =
  let h = (envelopeHeader e'') {headerMsgType = messageType (Proxy :: Proxy a)}
      e' = e'' {envelopeHeader = h}
      e = fixEnvelopeCheckSum $ fixEnvelopeBodyLength e'
   in Message
        { messageFields =
            concat
              -- TODO figure out what to do about the message type being in the
              -- header already
              [ renderHeader (envelopeHeader e),
                toComponentFields (envelopeContents e),
                renderTrailer (envelopeTrailer e)
              ]
        }

-- Has to happen _before_ fixEnvelopeCheckSum
fixEnvelopeBodyLength :: (IsMessage a) => Envelope a -> Envelope a
fixEnvelopeBodyLength e =
  let bodyLength = computeBodyLength e
   in e {envelopeHeader = (envelopeHeader e) {headerBodyLength = bodyLength}}

computeBodyLength :: (IsMessage a) => Envelope a -> BodyLength
computeBodyLength Envelope {..} =
  let bytesBeforeBodyLength =
        computeFieldsLength
          [ fieldB $ headerBeginString envelopeHeader,
            fieldB $ headerBodyLength envelopeHeader
          ]
      allFields =
        concat
          [ renderHeader envelopeHeader,
            toComponentFields envelopeContents,
            renderTrailer envelopeTrailer
          ]
      bytesFromCheckSum = computeFieldsLength [fieldB $ trailerCheckSum envelopeTrailer]
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
          [ renderHeader envelopeHeader,
            toComponentFields envelopeContents
          ]
      checkSum = computeCheckSum fieldsUntilCheckSum
   in e {envelopeTrailer = envelopeTrailer {trailerCheckSum = checkSum}}

computeCheckSum :: [Field] -> CheckSum
computeCheckSum fields =
  let bytes = renderMessage (Message {messageFields = fields})
      w = sum $ SB.unpack bytes
   in renderCheckSum w

renderCheckSum :: Word8 -> CheckSum
renderCheckSum = CheckSum . SimpleBytes . TE.encodeUtf8 . T.pack . printf "%03d"
