{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SecurityRequestResult where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 560, fieldName = "SecurityRequestResult", fieldType = FieldTypeInt, fieldValues = [FieldValueSpec {fieldValueEnum = "0", fieldValueDescription = "VALID_REQUEST"},FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "INVALID_OR_UNSUPPORTED_REQUEST"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "NO_INSTRUMENTS_FOUND"},FieldValueSpec {fieldValueEnum = "3", fieldValueDescription = "NOT_AUTHORIZED_TO_RETRIEVE_INSTRUMENT_DATA"},FieldValueSpec {fieldValueEnum = "4", fieldValueDescription = "INSTRUMENT_DATA_TEMPORARILY_UNAVAILABLE"},FieldValueSpec {fieldValueEnum = "5", fieldValueDescription = "REQUEST_FOR_INSTRUMENT_DATA_NOT_SUPPORTED"}]}
data SecurityRequestResult
  = SecurityRequestResult_VALID_REQUEST
  | SecurityRequestResult_INVALID_OR_UNSUPPORTED_REQUEST
  | SecurityRequestResult_NO_INSTRUMENTS_FOUND
  | SecurityRequestResult_NOT_AUTHORIZED_TO_RETRIEVE_INSTRUMENT_DATA
  | SecurityRequestResult_INSTRUMENT_DATA_TEMPORARILY_UNAVAILABLE
  | SecurityRequestResult_REQUEST_FOR_INSTRUMENT_DATA_NOT_SUPPORTED
  deriving stock (Show, Eq, Generic)

instance Validity SecurityRequestResult

instance IsField SecurityRequestResult where
  fieldTag Proxy = 560
  fieldIsData Proxy = False
  fieldToValue = \case
    SecurityRequestResult_VALID_REQUEST -> "0"
    SecurityRequestResult_INVALID_OR_UNSUPPORTED_REQUEST -> "1"
    SecurityRequestResult_NO_INSTRUMENTS_FOUND -> "2"
    SecurityRequestResult_NOT_AUTHORIZED_TO_RETRIEVE_INSTRUMENT_DATA -> "3"
    SecurityRequestResult_INSTRUMENT_DATA_TEMPORARILY_UNAVAILABLE -> "4"
    SecurityRequestResult_REQUEST_FOR_INSTRUMENT_DATA_NOT_SUPPORTED -> "5"
  fieldFromValue = \case
    "0" -> Right SecurityRequestResult_VALID_REQUEST
    "1" -> Right SecurityRequestResult_INVALID_OR_UNSUPPORTED_REQUEST
    "2" -> Right SecurityRequestResult_NO_INSTRUMENTS_FOUND
    "3" -> Right SecurityRequestResult_NOT_AUTHORIZED_TO_RETRIEVE_INSTRUMENT_DATA
    "4" -> Right SecurityRequestResult_INSTRUMENT_DATA_TEMPORARILY_UNAVAILABLE
    "5" -> Right SecurityRequestResult_REQUEST_FOR_INSTRUMENT_DATA_NOT_SUPPORTED
    v -> Left ("Unknown SecurityRequestResult: " <> show v)
