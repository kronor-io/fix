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

-- | FieldSpec {fieldNumber = 347, fieldName = "MessageEncoding", fieldType = FieldTypeString, fieldValues = [FieldValueSpec {fieldValueEnum = "ISO-2022-JP", fieldValueDescription = "ISO2022_JP"},FieldValueSpec {fieldValueEnum = "EUC-JP", fieldValueDescription = "EUCJP"},FieldValueSpec {fieldValueEnum = "Shift_JIS", fieldValueDescription = "SHIFT_JIS"},FieldValueSpec {fieldValueEnum = "UTF-8", fieldValueDescription = "UTF8"}]}
data MessageEncoding
  = MessageEncoding_ISO2022_JP
  | MessageEncoding_EUCJP
  | MessageEncoding_SHIFT_JIS
  | MessageEncoding_UTF8
  deriving stock (Show, Eq, Generic)

instance Validity MessageEncoding

instance IsField MessageEncoding where
  fieldTag Proxy = 347
  fieldIsData Proxy = False
  fieldToValue = \case
    MessageEncoding_ISO2022_JP -> "ISO-2022-JP"
    MessageEncoding_EUCJP -> "EUC-JP"
    MessageEncoding_SHIFT_JIS -> "Shift_JIS"
    MessageEncoding_UTF8 -> "UTF-8"
  fieldFromValue = \case
    "ISO-2022-JP" -> Right MessageEncoding_ISO2022_JP
    "EUC-JP" -> Right MessageEncoding_EUCJP
    "Shift_JIS" -> Right MessageEncoding_SHIFT_JIS
    "UTF-8" -> Right MessageEncoding_UTF8
    v -> Left ("Unknown MessageEncoding: " <> show v)
