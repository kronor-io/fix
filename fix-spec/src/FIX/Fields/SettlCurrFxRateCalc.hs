{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SettlCurrFxRateCalc where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 156, fieldName = "SettlCurrFxRateCalc", fieldType = FieldTypeChar, fieldValues = [FieldValueSpec {fieldValueEnum = "M", fieldValueDescription = "MULTIPLY"},FieldValueSpec {fieldValueEnum = "D", fieldValueDescription = "DIVIDE"}]}
data SettlCurrFxRateCalc
  = SettlCurrFxRateCalc_MULTIPLY
  | SettlCurrFxRateCalc_DIVIDE
  deriving stock (Show, Eq, Generic)

instance Validity SettlCurrFxRateCalc

instance IsField SettlCurrFxRateCalc where
  fieldTag Proxy = 156
  fieldIsData Proxy = False
  fieldToValue = \case
    SettlCurrFxRateCalc_MULTIPLY -> "M"
    SettlCurrFxRateCalc_DIVIDE -> "D"
  fieldFromValue = \case
    "M" -> Right SettlCurrFxRateCalc_MULTIPLY
    "D" -> Right SettlCurrFxRateCalc_DIVIDE
    v -> Left ("Unknown SettlCurrFxRateCalc: " <> show v)
