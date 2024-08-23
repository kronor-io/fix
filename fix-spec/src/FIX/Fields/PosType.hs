{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.PosType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 703, fieldName = "PosType", fieldType = FieldTypeString, fieldValues = [FieldValueSpec {fieldValueEnum = "TQ", fieldValueDescription = "TRANSACTION_QUANTITY"},FieldValueSpec {fieldValueEnum = "IAS", fieldValueDescription = "INTRA_SPREAD_QTY"},FieldValueSpec {fieldValueEnum = "IES", fieldValueDescription = "INTER_SPREAD_QTY"},FieldValueSpec {fieldValueEnum = "FIN", fieldValueDescription = "END_OF_DAY_QTY"},FieldValueSpec {fieldValueEnum = "SOD", fieldValueDescription = "START_OF_DAY_QTY"},FieldValueSpec {fieldValueEnum = "EX", fieldValueDescription = "OPTION_EXERCISE_QTY"},FieldValueSpec {fieldValueEnum = "AS", fieldValueDescription = "OPTION_ASSIGNMENT"},FieldValueSpec {fieldValueEnum = "TX", fieldValueDescription = "TRANSACTION_FROM_EXERCISE"},FieldValueSpec {fieldValueEnum = "TA", fieldValueDescription = "TRANSACTION_FROM_ASSIGNMENT"},FieldValueSpec {fieldValueEnum = "PIT", fieldValueDescription = "PIT_TRADE_QTY"},FieldValueSpec {fieldValueEnum = "TRF", fieldValueDescription = "TRANSFER_TRADE_QTY"},FieldValueSpec {fieldValueEnum = "ETR", fieldValueDescription = "ELECTRONIC_TRADE_QTY"},FieldValueSpec {fieldValueEnum = "ALC", fieldValueDescription = "ALLOCATION_TRADE_QTY"},FieldValueSpec {fieldValueEnum = "PA", fieldValueDescription = "ADJUSTMENT_QTY"},FieldValueSpec {fieldValueEnum = "ASF", fieldValueDescription = "AS_OF_TRADE_QTY"},FieldValueSpec {fieldValueEnum = "DLV", fieldValueDescription = "DELIVERY_QTY"},FieldValueSpec {fieldValueEnum = "TOT", fieldValueDescription = "TOTAL_TRANSACTION_QTY"},FieldValueSpec {fieldValueEnum = "XM", fieldValueDescription = "CROSS_MARGIN_QTY"},FieldValueSpec {fieldValueEnum = "SPL", fieldValueDescription = "INTEGRAL_SPLIT"}]}
data PosType
  = PosType_TRANSACTION_QUANTITY
  | PosType_INTRA_SPREAD_QTY
  | PosType_INTER_SPREAD_QTY
  | PosType_END_OF_DAY_QTY
  | PosType_START_OF_DAY_QTY
  | PosType_OPTION_EXERCISE_QTY
  | PosType_OPTION_ASSIGNMENT
  | PosType_TRANSACTION_FROM_EXERCISE
  | PosType_TRANSACTION_FROM_ASSIGNMENT
  | PosType_PIT_TRADE_QTY
  | PosType_TRANSFER_TRADE_QTY
  | PosType_ELECTRONIC_TRADE_QTY
  | PosType_ALLOCATION_TRADE_QTY
  | PosType_ADJUSTMENT_QTY
  | PosType_AS_OF_TRADE_QTY
  | PosType_DELIVERY_QTY
  | PosType_TOTAL_TRANSACTION_QTY
  | PosType_CROSS_MARGIN_QTY
  | PosType_INTEGRAL_SPLIT
  deriving stock (Show, Eq, Generic)

instance Validity PosType

instance IsField PosType where
  fieldTag Proxy = 703
  fieldIsData Proxy = False
  fieldToValue = \case
    PosType_TRANSACTION_QUANTITY -> "TQ"
    PosType_INTRA_SPREAD_QTY -> "IAS"
    PosType_INTER_SPREAD_QTY -> "IES"
    PosType_END_OF_DAY_QTY -> "FIN"
    PosType_START_OF_DAY_QTY -> "SOD"
    PosType_OPTION_EXERCISE_QTY -> "EX"
    PosType_OPTION_ASSIGNMENT -> "AS"
    PosType_TRANSACTION_FROM_EXERCISE -> "TX"
    PosType_TRANSACTION_FROM_ASSIGNMENT -> "TA"
    PosType_PIT_TRADE_QTY -> "PIT"
    PosType_TRANSFER_TRADE_QTY -> "TRF"
    PosType_ELECTRONIC_TRADE_QTY -> "ETR"
    PosType_ALLOCATION_TRADE_QTY -> "ALC"
    PosType_ADJUSTMENT_QTY -> "PA"
    PosType_AS_OF_TRADE_QTY -> "ASF"
    PosType_DELIVERY_QTY -> "DLV"
    PosType_TOTAL_TRANSACTION_QTY -> "TOT"
    PosType_CROSS_MARGIN_QTY -> "XM"
    PosType_INTEGRAL_SPLIT -> "SPL"
  fieldFromValue = \case
    "TQ" -> Right PosType_TRANSACTION_QUANTITY
    "IAS" -> Right PosType_INTRA_SPREAD_QTY
    "IES" -> Right PosType_INTER_SPREAD_QTY
    "FIN" -> Right PosType_END_OF_DAY_QTY
    "SOD" -> Right PosType_START_OF_DAY_QTY
    "EX" -> Right PosType_OPTION_EXERCISE_QTY
    "AS" -> Right PosType_OPTION_ASSIGNMENT
    "TX" -> Right PosType_TRANSACTION_FROM_EXERCISE
    "TA" -> Right PosType_TRANSACTION_FROM_ASSIGNMENT
    "PIT" -> Right PosType_PIT_TRADE_QTY
    "TRF" -> Right PosType_TRANSFER_TRADE_QTY
    "ETR" -> Right PosType_ELECTRONIC_TRADE_QTY
    "ALC" -> Right PosType_ALLOCATION_TRADE_QTY
    "PA" -> Right PosType_ADJUSTMENT_QTY
    "ASF" -> Right PosType_AS_OF_TRADE_QTY
    "DLV" -> Right PosType_DELIVERY_QTY
    "TOT" -> Right PosType_TOTAL_TRANSACTION_QTY
    "XM" -> Right PosType_CROSS_MARGIN_QTY
    "SPL" -> Right PosType_INTEGRAL_SPLIT
    v -> Left ("Unknown PosType: " <> show v)
