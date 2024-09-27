{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.HandlInst where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 21
--   , fieldName = "HandlInst"
--   , fieldType = FieldTypeChar
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "1"
--           , fieldValueDescription = "AUTOMATED_EXECUTION_ORDER_PRIVATE"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "2"
--           , fieldValueDescription = "AUTOMATED_EXECUTION_ORDER_PUBLIC"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "3" , fieldValueDescription = "MANUAL_ORDER" }
--       ]
--   }
data HandlInst
  = HandlInstAutomatedExecutionOrderPrivate
  | HandlInstAutomatedExecutionOrderPublic
  | HandlInstManualOrder
  deriving stock (Show, Eq, Generic)

instance Validity HandlInst

instance IsField HandlInst where
  fieldTag Proxy = 21
  fieldIsData Proxy = False
  fieldToValue = \case
    HandlInstAutomatedExecutionOrderPrivate -> "1"
    HandlInstAutomatedExecutionOrderPublic -> "2"
    HandlInstManualOrder -> "3"
  fieldFromValue = \case
    "1" -> Right HandlInstAutomatedExecutionOrderPrivate
    "2" -> Right HandlInstAutomatedExecutionOrderPublic
    "3" -> Right HandlInstManualOrder
    v -> Left ("Unknown HandlInst: " <> show v)
