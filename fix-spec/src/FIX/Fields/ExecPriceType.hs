{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.ExecPriceType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 484
--   , fieldName = "ExecPriceType"
--   , fieldType = FieldTypeChar
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "B" , fieldValueDescription = "BID_PRICE" }
--       , FieldValueSpec
--           { fieldValueEnum = "C" , fieldValueDescription = "CREATION_PRICE" }
--       , FieldValueSpec
--           { fieldValueEnum = "D"
--           , fieldValueDescription = "CREATION_PRICE_PLUS_ADJUSTMENT_PERCENT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "E"
--           , fieldValueDescription = "CREATION_PRICE_PLUS_ADJUSTMENT_AMOUNT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "O" , fieldValueDescription = "OFFER_PRICE" }
--       , FieldValueSpec
--           { fieldValueEnum = "P"
--           , fieldValueDescription = "OFFER_PRICE_MINUS_ADJUSTMENT_PERCENT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "Q"
--           , fieldValueDescription = "OFFER_PRICE_MINUS_ADJUSTMENT_AMOUNT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "S" , fieldValueDescription = "SINGLE_PRICE" }
--       ]
--   }
data ExecPriceType
  = ExecPriceTypeBidPrice
  | ExecPriceTypeCreationPrice
  | ExecPriceTypeCreationPricePlusAdjustmentPercent
  | ExecPriceTypeCreationPricePlusAdjustmentAmount
  | ExecPriceTypeOfferPrice
  | ExecPriceTypeOfferPriceMinusAdjustmentPercent
  | ExecPriceTypeOfferPriceMinusAdjustmentAmount
  | ExecPriceTypeSinglePrice
  deriving stock (Show, Eq, Generic)

instance Validity ExecPriceType

instance IsField ExecPriceType where
  fieldTag Proxy = 484
  fieldIsData Proxy = False
  fieldToValue = \case
    ExecPriceTypeBidPrice -> "B"
    ExecPriceTypeCreationPrice -> "C"
    ExecPriceTypeCreationPricePlusAdjustmentPercent -> "D"
    ExecPriceTypeCreationPricePlusAdjustmentAmount -> "E"
    ExecPriceTypeOfferPrice -> "O"
    ExecPriceTypeOfferPriceMinusAdjustmentPercent -> "P"
    ExecPriceTypeOfferPriceMinusAdjustmentAmount -> "Q"
    ExecPriceTypeSinglePrice -> "S"
  fieldFromValue = \case
    "B" -> Right ExecPriceTypeBidPrice
    "C" -> Right ExecPriceTypeCreationPrice
    "D" -> Right ExecPriceTypeCreationPricePlusAdjustmentPercent
    "E" -> Right ExecPriceTypeCreationPricePlusAdjustmentAmount
    "O" -> Right ExecPriceTypeOfferPrice
    "P" -> Right ExecPriceTypeOfferPriceMinusAdjustmentPercent
    "Q" -> Right ExecPriceTypeOfferPriceMinusAdjustmentAmount
    "S" -> Right ExecPriceTypeSinglePrice
    v -> Left ("Unknown ExecPriceType: " <> show v)
