{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.PosAmtType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 707, fieldName = "PosAmtType", fieldType = FieldTypeString, fieldValues = [FieldValueSpec {fieldValueEnum = "FMTM", fieldValueDescription = "FINAL_MARK_TO_MARKET_AMOUNT"},FieldValueSpec {fieldValueEnum = "IMTM", fieldValueDescription = "INCREMENTAL_MARK_TO_MARKET_AMOUNT"},FieldValueSpec {fieldValueEnum = "TVAR", fieldValueDescription = "TRADE_VARIATION_AMOUNT"},FieldValueSpec {fieldValueEnum = "SMTM", fieldValueDescription = "START_OF_DAY_MARK_TO_MARKET_AMOUNT"},FieldValueSpec {fieldValueEnum = "PREM", fieldValueDescription = "PREMIUM_AMOUNT"},FieldValueSpec {fieldValueEnum = "CRES", fieldValueDescription = "CASH_RESIDUAL_AMOUNT"},FieldValueSpec {fieldValueEnum = "CASH", fieldValueDescription = "CASH_AMOUNT"},FieldValueSpec {fieldValueEnum = "VADJ", fieldValueDescription = "VALUE_ADJUSTED_AMOUNT"}]}
data PosAmtType
  = PosAmtType_FINAL_MARK_TO_MARKET_AMOUNT
  | PosAmtType_INCREMENTAL_MARK_TO_MARKET_AMOUNT
  | PosAmtType_TRADE_VARIATION_AMOUNT
  | PosAmtType_START_OF_DAY_MARK_TO_MARKET_AMOUNT
  | PosAmtType_PREMIUM_AMOUNT
  | PosAmtType_CASH_RESIDUAL_AMOUNT
  | PosAmtType_CASH_AMOUNT
  | PosAmtType_VALUE_ADJUSTED_AMOUNT
  deriving stock (Show, Eq, Generic)

instance Validity PosAmtType

instance IsField PosAmtType where
  fieldTag Proxy = 707
  fieldIsData Proxy = False
  fieldToValue = \case
    PosAmtType_FINAL_MARK_TO_MARKET_AMOUNT -> "FMTM"
    PosAmtType_INCREMENTAL_MARK_TO_MARKET_AMOUNT -> "IMTM"
    PosAmtType_TRADE_VARIATION_AMOUNT -> "TVAR"
    PosAmtType_START_OF_DAY_MARK_TO_MARKET_AMOUNT -> "SMTM"
    PosAmtType_PREMIUM_AMOUNT -> "PREM"
    PosAmtType_CASH_RESIDUAL_AMOUNT -> "CRES"
    PosAmtType_CASH_AMOUNT -> "CASH"
    PosAmtType_VALUE_ADJUSTED_AMOUNT -> "VADJ"
  fieldFromValue = \case
    "FMTM" -> Right PosAmtType_FINAL_MARK_TO_MARKET_AMOUNT
    "IMTM" -> Right PosAmtType_INCREMENTAL_MARK_TO_MARKET_AMOUNT
    "TVAR" -> Right PosAmtType_TRADE_VARIATION_AMOUNT
    "SMTM" -> Right PosAmtType_START_OF_DAY_MARK_TO_MARKET_AMOUNT
    "PREM" -> Right PosAmtType_PREMIUM_AMOUNT
    "CRES" -> Right PosAmtType_CASH_RESIDUAL_AMOUNT
    "CASH" -> Right PosAmtType_CASH_AMOUNT
    "VADJ" -> Right PosAmtType_VALUE_ADJUSTED_AMOUNT
    v -> Left ("Unknown PosAmtType: " <> show v)
