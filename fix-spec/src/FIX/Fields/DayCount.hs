{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.DayCount where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 7072
--   , fieldName = "DayCount"
--   , fieldType = FieldTypeString
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "1/1" , fieldValueDescription = "1/1" }
--       , FieldValueSpec
--           { fieldValueEnum = "ACT/365F"
--           , fieldValueDescription = "ACT/365F"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "ACT/360" , fieldValueDescription = "ACT/360" }
--       , FieldValueSpec
--           { fieldValueEnum = "ACT/ACT" , fieldValueDescription = "ACT/ACT" }
--       , FieldValueSpec
--           { fieldValueEnum = "30/360" , fieldValueDescription = "30/360" }
--       , FieldValueSpec
--           { fieldValueEnum = "30E/360" , fieldValueDescription = "30E/360" }
--       , FieldValueSpec
--           { fieldValueEnum = "BUS/252" , fieldValueDescription = "BUS/252" }
--       ]
--   }
data DayCount
  = DayCount11
  | DayCountAct365f
  | DayCountAct360
  | DayCountActAct
  | DayCount30360
  | DayCount30e360
  | DayCountBus252
  deriving stock (Show, Eq, Generic)

instance Validity DayCount

instance IsField DayCount where
  fieldTag Proxy = 7072
  fieldIsData Proxy = False
  fieldToValue = \case
    DayCount11 -> "1/1"
    DayCountAct365f -> "ACT/365F"
    DayCountAct360 -> "ACT/360"
    DayCountActAct -> "ACT/ACT"
    DayCount30360 -> "30/360"
    DayCount30e360 -> "30E/360"
    DayCountBus252 -> "BUS/252"
  fieldFromValue = \case
    "1/1" -> Right DayCount11
    "ACT/365F" -> Right DayCountAct365f
    "ACT/360" -> Right DayCountAct360
    "ACT/ACT" -> Right DayCountActAct
    "30/360" -> Right DayCount30360
    "30E/360" -> Right DayCount30e360
    "BUS/252" -> Right DayCountBus252
    v -> Left ("Unknown DayCount: " <> show v)