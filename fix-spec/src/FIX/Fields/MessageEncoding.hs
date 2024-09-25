{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.MessageEncoding where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 347
--   , fieldName = "MessageEncoding"
--   , fieldType = FieldTypeString
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "ISO-2022-JP"
--           , fieldValueDescription = "ISO_2022_JP"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "EUC-JP" , fieldValueDescription = "EUC_JP" }
--       , FieldValueSpec
--           { fieldValueEnum = "SHIFT_JIS"
--           , fieldValueDescription = "SHIFT_JIS"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "UTF-8" , fieldValueDescription = "UTF_8" }
--       ]
--   }
data MessageEncoding
  = MessageEncodingIso2022Jp
  | MessageEncodingEucJp
  | MessageEncodingShiftJis
  | MessageEncodingUtf8
  deriving stock (Show, Eq, Generic)

instance Validity MessageEncoding

instance IsField MessageEncoding where
  fieldTag Proxy = 347
  fieldIsData Proxy = False
  fieldToValue = \case
    MessageEncodingIso2022Jp -> "ISO-2022-JP"
    MessageEncodingEucJp -> "EUC-JP"
    MessageEncodingShiftJis -> "SHIFT_JIS"
    MessageEncodingUtf8 -> "UTF-8"
  fieldFromValue = \case
    "ISO-2022-JP" -> Right MessageEncodingIso2022Jp
    "EUC-JP" -> Right MessageEncodingEucJp
    "SHIFT_JIS" -> Right MessageEncodingShiftJis
    "UTF-8" -> Right MessageEncodingUtf8
    v -> Left ("Unknown MessageEncoding: " <> show v)
