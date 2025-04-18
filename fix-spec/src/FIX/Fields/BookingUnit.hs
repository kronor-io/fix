{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.BookingUnit where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 590
--   , fieldName = "BookingUnit"
--   , fieldType = FieldTypeChar
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "0"
--           , fieldValueDescription =
--               "EACH_PARTIAL_EXECUTION_IS_A_BOOKABLE_UNIT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "1"
--           , fieldValueDescription =
--               "AGGREGATE_PARTIAL_EXECUTIONS_ON_THIS_ORDER_AND_BOOK_ONE_TRADE_PER_ORDER"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "2"
--           , fieldValueDescription =
--               "AGGREGATE_EXECUTIONS_FOR_THIS_SYMBOL_SIDE_AND_SETTLEMENT_DATE"
--           }
--       ]
--   }
data BookingUnit
  = BookingUnitEachPartialExecutionIsABookableUnit
  | BookingUnitAggregatePartialExecutionsOnThisOrderAndBookOneTradePerOrder
  | BookingUnitAggregateExecutionsForThisSymbolSideAndSettlementDate
  deriving stock (Show, Eq, Generic)

instance Validity BookingUnit

instance IsField BookingUnit where
  fieldTag Proxy = 590
  fieldIsData Proxy = False
  fieldToValue = \case
    BookingUnitEachPartialExecutionIsABookableUnit -> "0"
    BookingUnitAggregatePartialExecutionsOnThisOrderAndBookOneTradePerOrder -> "1"
    BookingUnitAggregateExecutionsForThisSymbolSideAndSettlementDate -> "2"
  fieldFromValue = \case
    "0" -> Right BookingUnitEachPartialExecutionIsABookableUnit
    "1" -> Right BookingUnitAggregatePartialExecutionsOnThisOrderAndBookOneTradePerOrder
    "2" -> Right BookingUnitAggregateExecutionsForThisSymbolSideAndSettlementDate
    v -> Left ("Unknown BookingUnit: " <> show v)
