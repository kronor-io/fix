{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.OrdStatus where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 39, fieldName = "OrdStatus", fieldType = FieldTypeChar, fieldValues = [FieldValueSpec {fieldValueEnum = "0", fieldValueDescription = "NEW"},FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "PARTIALLY_FILLED"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "FILLED"},FieldValueSpec {fieldValueEnum = "3", fieldValueDescription = "DONE_FOR_DAY"},FieldValueSpec {fieldValueEnum = "4", fieldValueDescription = "CANCELED"},FieldValueSpec {fieldValueEnum = "6", fieldValueDescription = "PENDING_CANCEL"},FieldValueSpec {fieldValueEnum = "7", fieldValueDescription = "STOPPED"},FieldValueSpec {fieldValueEnum = "8", fieldValueDescription = "REJECTED"},FieldValueSpec {fieldValueEnum = "9", fieldValueDescription = "SUSPENDED"},FieldValueSpec {fieldValueEnum = "A", fieldValueDescription = "PENDING_NEW"},FieldValueSpec {fieldValueEnum = "B", fieldValueDescription = "CALCULATED"},FieldValueSpec {fieldValueEnum = "C", fieldValueDescription = "EXPIRED"},FieldValueSpec {fieldValueEnum = "D", fieldValueDescription = "ACCEPTED_FOR_BIDDING"},FieldValueSpec {fieldValueEnum = "E", fieldValueDescription = "PENDING_REPLACE"}]}
data OrdStatus
  = OrdStatus_NEW
  | OrdStatus_PARTIALLY_FILLED
  | OrdStatus_FILLED
  | OrdStatus_DONE_FOR_DAY
  | OrdStatus_CANCELED
  | OrdStatus_PENDING_CANCEL
  | OrdStatus_STOPPED
  | OrdStatus_REJECTED
  | OrdStatus_SUSPENDED
  | OrdStatus_PENDING_NEW
  | OrdStatus_CALCULATED
  | OrdStatus_EXPIRED
  | OrdStatus_ACCEPTED_FOR_BIDDING
  | OrdStatus_PENDING_REPLACE
  deriving stock (Show, Eq, Generic)

instance Validity OrdStatus

instance IsField OrdStatus where
  fieldTag Proxy = 39
  fieldIsData Proxy = False
  fieldToValue = \case
    OrdStatus_NEW -> "0"
    OrdStatus_PARTIALLY_FILLED -> "1"
    OrdStatus_FILLED -> "2"
    OrdStatus_DONE_FOR_DAY -> "3"
    OrdStatus_CANCELED -> "4"
    OrdStatus_PENDING_CANCEL -> "6"
    OrdStatus_STOPPED -> "7"
    OrdStatus_REJECTED -> "8"
    OrdStatus_SUSPENDED -> "9"
    OrdStatus_PENDING_NEW -> "A"
    OrdStatus_CALCULATED -> "B"
    OrdStatus_EXPIRED -> "C"
    OrdStatus_ACCEPTED_FOR_BIDDING -> "D"
    OrdStatus_PENDING_REPLACE -> "E"
  fieldFromValue = \case
    "0" -> Right OrdStatus_NEW
    "1" -> Right OrdStatus_PARTIALLY_FILLED
    "2" -> Right OrdStatus_FILLED
    "3" -> Right OrdStatus_DONE_FOR_DAY
    "4" -> Right OrdStatus_CANCELED
    "6" -> Right OrdStatus_PENDING_CANCEL
    "7" -> Right OrdStatus_STOPPED
    "8" -> Right OrdStatus_REJECTED
    "9" -> Right OrdStatus_SUSPENDED
    "A" -> Right OrdStatus_PENDING_NEW
    "B" -> Right OrdStatus_CALCULATED
    "C" -> Right OrdStatus_EXPIRED
    "D" -> Right OrdStatus_ACCEPTED_FOR_BIDDING
    "E" -> Right OrdStatus_PENDING_REPLACE
    v -> Left ("Unknown OrdStatus: " <> show v)
