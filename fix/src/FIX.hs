{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module FIX where

import Data.ByteString (ByteString)
import Data.Validity
import Data.Validity.ByteString ()
import GHC.Generics (Generic)

data Message = Message
  deriving (Show, Eq, Generic)

instance Validity Message

parseMessage :: ByteString -> Either String Message
parseMessage _ = Right Message

renderMessage :: Message -> ByteString
renderMessage Message = ""
