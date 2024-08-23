{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.QuoteType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 537, fieldName = "QuoteType", fieldType = FieldTypeInt, fieldValues = [FieldValueSpec {fieldValueEnum = "0", fieldValueDescription = "INDICATIVE"},FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "TRADEABLE"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "RESTRICTED_TRADEABLE"},FieldValueSpec {fieldValueEnum = "3", fieldValueDescription = "COUNTER"}]}
data QuoteType
  = QuoteType_INDICATIVE
  | QuoteType_TRADEABLE
  | QuoteType_RESTRICTED_TRADEABLE
  | QuoteType_COUNTER
  deriving stock (Show, Eq, Generic)

instance Validity QuoteType

instance IsField QuoteType where
  fieldTag Proxy = 537
  fieldIsData Proxy = False
  fieldToValue = \case
    QuoteType_INDICATIVE -> "0"
    QuoteType_TRADEABLE -> "1"
    QuoteType_RESTRICTED_TRADEABLE -> "2"
    QuoteType_COUNTER -> "3"
  fieldFromValue = \case
    "0" -> Right QuoteType_INDICATIVE
    "1" -> Right QuoteType_TRADEABLE
    "2" -> Right QuoteType_RESTRICTED_TRADEABLE
    "3" -> Right QuoteType_COUNTER
    v -> Left ("Unknown QuoteType: " <> show v)
