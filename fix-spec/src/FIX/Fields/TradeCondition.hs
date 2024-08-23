{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.TradeCondition where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 277, fieldName = "TradeCondition", fieldType = FieldTypeMultipleValueString, fieldValues = [FieldValueSpec {fieldValueEnum = "A", fieldValueDescription = "CASH"},FieldValueSpec {fieldValueEnum = "B", fieldValueDescription = "AVERAGE_PRICE_TRADE"},FieldValueSpec {fieldValueEnum = "C", fieldValueDescription = "CASH_TRADE"},FieldValueSpec {fieldValueEnum = "D", fieldValueDescription = "NEXT_DAY"},FieldValueSpec {fieldValueEnum = "E", fieldValueDescription = "OPENING"},FieldValueSpec {fieldValueEnum = "F", fieldValueDescription = "INTRADAY_TRADE_DETAIL"},FieldValueSpec {fieldValueEnum = "G", fieldValueDescription = "RULE127_TRADE"},FieldValueSpec {fieldValueEnum = "H", fieldValueDescription = "RULE155_TRADE"},FieldValueSpec {fieldValueEnum = "I", fieldValueDescription = "SOLD_LAST"},FieldValueSpec {fieldValueEnum = "J", fieldValueDescription = "NEXT_DAY_TRADE"},FieldValueSpec {fieldValueEnum = "K", fieldValueDescription = "OPENED"},FieldValueSpec {fieldValueEnum = "L", fieldValueDescription = "SELLER"},FieldValueSpec {fieldValueEnum = "M", fieldValueDescription = "SOLD"},FieldValueSpec {fieldValueEnum = "N", fieldValueDescription = "STOPPED_STOCK"},FieldValueSpec {fieldValueEnum = "P", fieldValueDescription = "IMBALANCE_MORE_BUYERS"},FieldValueSpec {fieldValueEnum = "Q", fieldValueDescription = "IMBALANCE_MORE_SELLERS"},FieldValueSpec {fieldValueEnum = "R", fieldValueDescription = "OPENING_PRICE"}]}
data TradeCondition
  = TradeCondition_CASH
  | TradeCondition_AVERAGE_PRICE_TRADE
  | TradeCondition_CASH_TRADE
  | TradeCondition_NEXT_DAY
  | TradeCondition_OPENING
  | TradeCondition_INTRADAY_TRADE_DETAIL
  | TradeCondition_RULE127_TRADE
  | TradeCondition_RULE155_TRADE
  | TradeCondition_SOLD_LAST
  | TradeCondition_NEXT_DAY_TRADE
  | TradeCondition_OPENED
  | TradeCondition_SELLER
  | TradeCondition_SOLD
  | TradeCondition_STOPPED_STOCK
  | TradeCondition_IMBALANCE_MORE_BUYERS
  | TradeCondition_IMBALANCE_MORE_SELLERS
  | TradeCondition_OPENING_PRICE
  deriving stock (Show, Eq, Generic)

instance Validity TradeCondition

instance IsField TradeCondition where
  fieldTag Proxy = 277
  fieldIsData Proxy = False
  fieldToValue = \case
    TradeCondition_CASH -> "A"
    TradeCondition_AVERAGE_PRICE_TRADE -> "B"
    TradeCondition_CASH_TRADE -> "C"
    TradeCondition_NEXT_DAY -> "D"
    TradeCondition_OPENING -> "E"
    TradeCondition_INTRADAY_TRADE_DETAIL -> "F"
    TradeCondition_RULE127_TRADE -> "G"
    TradeCondition_RULE155_TRADE -> "H"
    TradeCondition_SOLD_LAST -> "I"
    TradeCondition_NEXT_DAY_TRADE -> "J"
    TradeCondition_OPENED -> "K"
    TradeCondition_SELLER -> "L"
    TradeCondition_SOLD -> "M"
    TradeCondition_STOPPED_STOCK -> "N"
    TradeCondition_IMBALANCE_MORE_BUYERS -> "P"
    TradeCondition_IMBALANCE_MORE_SELLERS -> "Q"
    TradeCondition_OPENING_PRICE -> "R"
  fieldFromValue = \case
    "A" -> Right TradeCondition_CASH
    "B" -> Right TradeCondition_AVERAGE_PRICE_TRADE
    "C" -> Right TradeCondition_CASH_TRADE
    "D" -> Right TradeCondition_NEXT_DAY
    "E" -> Right TradeCondition_OPENING
    "F" -> Right TradeCondition_INTRADAY_TRADE_DETAIL
    "G" -> Right TradeCondition_RULE127_TRADE
    "H" -> Right TradeCondition_RULE155_TRADE
    "I" -> Right TradeCondition_SOLD_LAST
    "J" -> Right TradeCondition_NEXT_DAY_TRADE
    "K" -> Right TradeCondition_OPENED
    "L" -> Right TradeCondition_SELLER
    "M" -> Right TradeCondition_SOLD
    "N" -> Right TradeCondition_STOPPED_STOCK
    "P" -> Right TradeCondition_IMBALANCE_MORE_BUYERS
    "Q" -> Right TradeCondition_IMBALANCE_MORE_SELLERS
    "R" -> Right TradeCondition_OPENING_PRICE
    v -> Left ("Unknown TradeCondition: " <> show v)
