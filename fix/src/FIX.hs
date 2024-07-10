{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module FIX where

import Control.Arrow (left)
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder as ByteString
import qualified Data.ByteString.Lazy as LB
import Data.Validity
import Data.Validity.ByteString ()
import Data.Void
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer

newtype Message = Message
  { messageFields :: [(Word, ByteString)]
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
