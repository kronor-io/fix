{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SettlType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 63
--   , fieldName = "SettlType"
--   , fieldType = FieldTypeChar
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "0" , fieldValueDescription = "REGULAR" }
--       , FieldValueSpec
--           { fieldValueEnum = "1" , fieldValueDescription = "CASH" }
--       , FieldValueSpec
--           { fieldValueEnum = "2" , fieldValueDescription = "NEXT_DAY" }
--       , FieldValueSpec
--           { fieldValueEnum = "3" , fieldValueDescription = "T_PLUS_2" }
--       , FieldValueSpec
--           { fieldValueEnum = "4" , fieldValueDescription = "T_PLUS_3" }
--       , FieldValueSpec
--           { fieldValueEnum = "5" , fieldValueDescription = "T_PLUS_4" }
--       , FieldValueSpec
--           { fieldValueEnum = "6" , fieldValueDescription = "FUTURE" }
--       , FieldValueSpec
--           { fieldValueEnum = "7"
--           , fieldValueDescription = "WHEN_AND_IF_ISSUED"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "8" , fieldValueDescription = "SELLERS_OPTION" }
--       , FieldValueSpec
--           { fieldValueEnum = "9" , fieldValueDescription = "T_PLUS_5" }
--       ]
--   }
data SettlType
  = SettlTypeRegular
  | SettlTypeCash
  | SettlTypeNextDay
  | SettlTypeTPlus2
  | SettlTypeTPlus3
  | SettlTypeTPlus4
  | SettlTypeFuture
  | SettlTypeWhenAndIfIssued
  | SettlTypeSellersOption
  | SettlTypeTPlus5
  deriving stock (Show, Eq, Generic)

instance Validity SettlType

instance IsField SettlType where
  fieldTag Proxy = 63
  fieldIsData Proxy = False
  fieldToValue = \case
    SettlTypeRegular -> "0"
    SettlTypeCash -> "1"
    SettlTypeNextDay -> "2"
    SettlTypeTPlus2 -> "3"
    SettlTypeTPlus3 -> "4"
    SettlTypeTPlus4 -> "5"
    SettlTypeFuture -> "6"
    SettlTypeWhenAndIfIssued -> "7"
    SettlTypeSellersOption -> "8"
    SettlTypeTPlus5 -> "9"
  fieldFromValue = \case
    "0" -> Right SettlTypeRegular
    "1" -> Right SettlTypeCash
    "2" -> Right SettlTypeNextDay
    "3" -> Right SettlTypeTPlus2
    "4" -> Right SettlTypeTPlus3
    "5" -> Right SettlTypeTPlus4
    "6" -> Right SettlTypeFuture
    "7" -> Right SettlTypeWhenAndIfIssued
    "8" -> Right SettlTypeSellersOption
    "9" -> Right SettlTypeTPlus5
    v -> Left ("Unknown SettlType: " <> show v)
