{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.ContAmtType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 519, fieldName = "ContAmtType", fieldType = FieldTypeInt, fieldValues = [FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "COMMISSION_AMOUNT"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "COMMISSION_PERCENT"},FieldValueSpec {fieldValueEnum = "3", fieldValueDescription = "INITIAL_CHARGE_AMOUNT"},FieldValueSpec {fieldValueEnum = "4", fieldValueDescription = "INITIAL_CHARGE_PERCENT"},FieldValueSpec {fieldValueEnum = "5", fieldValueDescription = "DISCOUNT_AMOUNT"},FieldValueSpec {fieldValueEnum = "6", fieldValueDescription = "DISCOUNT_PERCENT"},FieldValueSpec {fieldValueEnum = "7", fieldValueDescription = "DILUTION_LEVY_AMOUNT"},FieldValueSpec {fieldValueEnum = "8", fieldValueDescription = "DILUTION_LEVY_PERCENT"},FieldValueSpec {fieldValueEnum = "9", fieldValueDescription = "EXIT_CHARGE_AMOUNT"},FieldValueSpec {fieldValueEnum = "10", fieldValueDescription = "EXIT_CHARGE_PERCENT"},FieldValueSpec {fieldValueEnum = "11", fieldValueDescription = "FUND_BASED_RENEWAL_COMMISSION_PERCENT"},FieldValueSpec {fieldValueEnum = "12", fieldValueDescription = "PROJECTED_FUND_VALUE"},FieldValueSpec {fieldValueEnum = "13", fieldValueDescription = "FUND_BASED_RENEWAL_COMMISSION_ON_ORDER"},FieldValueSpec {fieldValueEnum = "14", fieldValueDescription = "FUND_BASED_RENEWAL_COMMISSION_ON_FUND"},FieldValueSpec {fieldValueEnum = "15", fieldValueDescription = "NET_SETTLEMENT_AMOUNT"}]}
data ContAmtType
  = ContAmtType_COMMISSION_AMOUNT
  | ContAmtType_COMMISSION_PERCENT
  | ContAmtType_INITIAL_CHARGE_AMOUNT
  | ContAmtType_INITIAL_CHARGE_PERCENT
  | ContAmtType_DISCOUNT_AMOUNT
  | ContAmtType_DISCOUNT_PERCENT
  | ContAmtType_DILUTION_LEVY_AMOUNT
  | ContAmtType_DILUTION_LEVY_PERCENT
  | ContAmtType_EXIT_CHARGE_AMOUNT
  | ContAmtType_EXIT_CHARGE_PERCENT
  | ContAmtType_FUND_BASED_RENEWAL_COMMISSION_PERCENT
  | ContAmtType_PROJECTED_FUND_VALUE
  | ContAmtType_FUND_BASED_RENEWAL_COMMISSION_ON_ORDER
  | ContAmtType_FUND_BASED_RENEWAL_COMMISSION_ON_FUND
  | ContAmtType_NET_SETTLEMENT_AMOUNT
  deriving stock (Show, Eq, Generic)

instance Validity ContAmtType

instance IsField ContAmtType where
  fieldTag Proxy = 519
  fieldIsData Proxy = False
  fieldToValue = \case
    ContAmtType_COMMISSION_AMOUNT -> "1"
    ContAmtType_COMMISSION_PERCENT -> "2"
    ContAmtType_INITIAL_CHARGE_AMOUNT -> "3"
    ContAmtType_INITIAL_CHARGE_PERCENT -> "4"
    ContAmtType_DISCOUNT_AMOUNT -> "5"
    ContAmtType_DISCOUNT_PERCENT -> "6"
    ContAmtType_DILUTION_LEVY_AMOUNT -> "7"
    ContAmtType_DILUTION_LEVY_PERCENT -> "8"
    ContAmtType_EXIT_CHARGE_AMOUNT -> "9"
    ContAmtType_EXIT_CHARGE_PERCENT -> "10"
    ContAmtType_FUND_BASED_RENEWAL_COMMISSION_PERCENT -> "11"
    ContAmtType_PROJECTED_FUND_VALUE -> "12"
    ContAmtType_FUND_BASED_RENEWAL_COMMISSION_ON_ORDER -> "13"
    ContAmtType_FUND_BASED_RENEWAL_COMMISSION_ON_FUND -> "14"
    ContAmtType_NET_SETTLEMENT_AMOUNT -> "15"
  fieldFromValue = \case
    "1" -> Right ContAmtType_COMMISSION_AMOUNT
    "2" -> Right ContAmtType_COMMISSION_PERCENT
    "3" -> Right ContAmtType_INITIAL_CHARGE_AMOUNT
    "4" -> Right ContAmtType_INITIAL_CHARGE_PERCENT
    "5" -> Right ContAmtType_DISCOUNT_AMOUNT
    "6" -> Right ContAmtType_DISCOUNT_PERCENT
    "7" -> Right ContAmtType_DILUTION_LEVY_AMOUNT
    "8" -> Right ContAmtType_DILUTION_LEVY_PERCENT
    "9" -> Right ContAmtType_EXIT_CHARGE_AMOUNT
    "10" -> Right ContAmtType_EXIT_CHARGE_PERCENT
    "11" -> Right ContAmtType_FUND_BASED_RENEWAL_COMMISSION_PERCENT
    "12" -> Right ContAmtType_PROJECTED_FUND_VALUE
    "13" -> Right ContAmtType_FUND_BASED_RENEWAL_COMMISSION_ON_ORDER
    "14" -> Right ContAmtType_FUND_BASED_RENEWAL_COMMISSION_ON_FUND
    "15" -> Right ContAmtType_NET_SETTLEMENT_AMOUNT
    v -> Left ("Unknown ContAmtType: " <> show v)
