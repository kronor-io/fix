{-# LANGUAGE ScopedTypeVariables #-}

module FIX.Messages.Class where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as ByteString
import Data.Proxy
import Data.Void
import FIX.Components.Class
import FIX.Fields.MsgType
import Text.Megaparsec
import Text.Megaparsec.Byte.Lexer

class (IsComponent a) => IsMessage a where
  messageType :: Proxy a -> MsgType

messageB :: (IsMessage a) => a -> ByteString.Builder
messageB = undefined

messageP :: (IsMessage a) => MsgType -> Parsec Void ByteString a
messageP = undefined
