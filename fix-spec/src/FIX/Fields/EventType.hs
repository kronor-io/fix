{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.EventType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 865, fieldName = "EventType", fieldType = FieldTypeInt, fieldValues = [FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "PUT"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "CALL"},FieldValueSpec {fieldValueEnum = "3", fieldValueDescription = "TENDER"},FieldValueSpec {fieldValueEnum = "4", fieldValueDescription = "SINKING_FUND_CALL"},FieldValueSpec {fieldValueEnum = "99", fieldValueDescription = "OTHER"}]}
data EventType
  = EventType_PUT
  | EventType_CALL
  | EventType_TENDER
  | EventType_SINKING_FUND_CALL
  | EventType_OTHER
  deriving stock (Show, Eq, Generic)

instance Validity EventType

instance IsField EventType where
  fieldTag Proxy = 865
  fieldIsData Proxy = False
  fieldToValue = \case
    EventType_PUT -> "1"
    EventType_CALL -> "2"
    EventType_TENDER -> "3"
    EventType_SINKING_FUND_CALL -> "4"
    EventType_OTHER -> "99"
  fieldFromValue = \case
    "1" -> Right EventType_PUT
    "2" -> Right EventType_CALL
    "3" -> Right EventType_TENDER
    "4" -> Right EventType_SINKING_FUND_CALL
    "99" -> Right EventType_OTHER
    v -> Left ("Unknown EventType: " <> show v)
