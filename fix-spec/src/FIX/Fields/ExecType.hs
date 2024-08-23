{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.ExecType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 150, fieldName = "ExecType", fieldType = FieldTypeChar, fieldValues = [FieldValueSpec {fieldValueEnum = "0", fieldValueDescription = "NEW"},FieldValueSpec {fieldValueEnum = "3", fieldValueDescription = "DONE_FOR_DAY"},FieldValueSpec {fieldValueEnum = "4", fieldValueDescription = "CANCELED"},FieldValueSpec {fieldValueEnum = "5", fieldValueDescription = "REPLACED"},FieldValueSpec {fieldValueEnum = "6", fieldValueDescription = "PENDING_CANCEL"},FieldValueSpec {fieldValueEnum = "7", fieldValueDescription = "STOPPED"},FieldValueSpec {fieldValueEnum = "8", fieldValueDescription = "REJECTED"},FieldValueSpec {fieldValueEnum = "9", fieldValueDescription = "SUSPENDED"},FieldValueSpec {fieldValueEnum = "A", fieldValueDescription = "PENDING_NEW"},FieldValueSpec {fieldValueEnum = "B", fieldValueDescription = "CALCULATED"},FieldValueSpec {fieldValueEnum = "C", fieldValueDescription = "EXPIRED"},FieldValueSpec {fieldValueEnum = "D", fieldValueDescription = "RESTATED"},FieldValueSpec {fieldValueEnum = "E", fieldValueDescription = "PENDING_REPLACE"},FieldValueSpec {fieldValueEnum = "F", fieldValueDescription = "TRADE"},FieldValueSpec {fieldValueEnum = "G", fieldValueDescription = "TRADE_CORRECT"},FieldValueSpec {fieldValueEnum = "H", fieldValueDescription = "TRADE_CANCEL"},FieldValueSpec {fieldValueEnum = "I", fieldValueDescription = "ORDER_STATUS"}]}
data ExecType
  = ExecType_NEW
  | ExecType_DONE_FOR_DAY
  | ExecType_CANCELED
  | ExecType_REPLACED
  | ExecType_PENDING_CANCEL
  | ExecType_STOPPED
  | ExecType_REJECTED
  | ExecType_SUSPENDED
  | ExecType_PENDING_NEW
  | ExecType_CALCULATED
  | ExecType_EXPIRED
  | ExecType_RESTATED
  | ExecType_PENDING_REPLACE
  | ExecType_TRADE
  | ExecType_TRADE_CORRECT
  | ExecType_TRADE_CANCEL
  | ExecType_ORDER_STATUS
  deriving stock (Show, Eq, Generic)

instance Validity ExecType

instance IsField ExecType where
  fieldTag Proxy = 150
  fieldIsData Proxy = False
  fieldToValue = \case
    ExecType_NEW -> "0"
    ExecType_DONE_FOR_DAY -> "3"
    ExecType_CANCELED -> "4"
    ExecType_REPLACED -> "5"
    ExecType_PENDING_CANCEL -> "6"
    ExecType_STOPPED -> "7"
    ExecType_REJECTED -> "8"
    ExecType_SUSPENDED -> "9"
    ExecType_PENDING_NEW -> "A"
    ExecType_CALCULATED -> "B"
    ExecType_EXPIRED -> "C"
    ExecType_RESTATED -> "D"
    ExecType_PENDING_REPLACE -> "E"
    ExecType_TRADE -> "F"
    ExecType_TRADE_CORRECT -> "G"
    ExecType_TRADE_CANCEL -> "H"
    ExecType_ORDER_STATUS -> "I"
  fieldFromValue = \case
    "0" -> Right ExecType_NEW
    "3" -> Right ExecType_DONE_FOR_DAY
    "4" -> Right ExecType_CANCELED
    "5" -> Right ExecType_REPLACED
    "6" -> Right ExecType_PENDING_CANCEL
    "7" -> Right ExecType_STOPPED
    "8" -> Right ExecType_REJECTED
    "9" -> Right ExecType_SUSPENDED
    "A" -> Right ExecType_PENDING_NEW
    "B" -> Right ExecType_CALCULATED
    "C" -> Right ExecType_EXPIRED
    "D" -> Right ExecType_RESTATED
    "E" -> Right ExecType_PENDING_REPLACE
    "F" -> Right ExecType_TRADE
    "G" -> Right ExecType_TRADE_CORRECT
    "H" -> Right ExecType_TRADE_CANCEL
    "I" -> Right ExecType_ORDER_STATUS
    v -> Left ("Unknown ExecType: " <> show v)
