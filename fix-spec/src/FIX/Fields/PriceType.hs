{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.PriceType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 423
--   , fieldName = "PriceType"
--   , fieldType = FieldTypeInt
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "1" , fieldValueDescription = "PERCENTAGE" }
--       , FieldValueSpec
--           { fieldValueEnum = "2" , fieldValueDescription = "PER_UNIT" }
--       , FieldValueSpec
--           { fieldValueEnum = "3" , fieldValueDescription = "FIXED_AMOUNT" }
--       , FieldValueSpec
--           { fieldValueEnum = "4" , fieldValueDescription = "DISCOUNT" }
--       , FieldValueSpec
--           { fieldValueEnum = "5" , fieldValueDescription = "PREMIUM" }
--       , FieldValueSpec
--           { fieldValueEnum = "6" , fieldValueDescription = "SPREAD" }
--       , FieldValueSpec
--           { fieldValueEnum = "7" , fieldValueDescription = "TED_PRICE" }
--       , FieldValueSpec
--           { fieldValueEnum = "8" , fieldValueDescription = "TED_YIELD" }
--       , FieldValueSpec
--           { fieldValueEnum = "9" , fieldValueDescription = "YIELD" }
--       ]
--   }
data PriceType
  = PriceTypePercentage
  | PriceTypePerUnit
  | PriceTypeFixedAmount
  | PriceTypeDiscount
  | PriceTypePremium
  | PriceTypeSpread
  | PriceTypeTedPrice
  | PriceTypeTedYield
  | PriceTypeYield
  deriving stock (Show, Eq, Generic)

instance Validity PriceType

instance IsField PriceType where
  fieldTag Proxy = 423
  fieldIsData Proxy = False
  fieldToValue = \case
    PriceTypePercentage -> "1"
    PriceTypePerUnit -> "2"
    PriceTypeFixedAmount -> "3"
    PriceTypeDiscount -> "4"
    PriceTypePremium -> "5"
    PriceTypeSpread -> "6"
    PriceTypeTedPrice -> "7"
    PriceTypeTedYield -> "8"
    PriceTypeYield -> "9"
  fieldFromValue = \case
    "1" -> Right PriceTypePercentage
    "2" -> Right PriceTypePerUnit
    "3" -> Right PriceTypeFixedAmount
    "4" -> Right PriceTypeDiscount
    "5" -> Right PriceTypePremium
    "6" -> Right PriceTypeSpread
    "7" -> Right PriceTypeTedPrice
    "8" -> Right PriceTypeTedYield
    "9" -> Right PriceTypeYield
    v -> Left ("Unknown PriceType: " <> show v)