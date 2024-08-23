{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.OrderCapacity where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 528, fieldName = "OrderCapacity", fieldType = FieldTypeChar, fieldValues = [FieldValueSpec {fieldValueEnum = "A", fieldValueDescription = "AGENCY"},FieldValueSpec {fieldValueEnum = "G", fieldValueDescription = "PROPRIETARY"},FieldValueSpec {fieldValueEnum = "I", fieldValueDescription = "INDIVIDUAL"},FieldValueSpec {fieldValueEnum = "P", fieldValueDescription = "PRINCIPAL"},FieldValueSpec {fieldValueEnum = "R", fieldValueDescription = "RISKLESS_PRINCIPAL"},FieldValueSpec {fieldValueEnum = "W", fieldValueDescription = "AGENT_FOR_OTHER_MEMBER"}]}
data OrderCapacity
  = OrderCapacity_AGENCY
  | OrderCapacity_PROPRIETARY
  | OrderCapacity_INDIVIDUAL
  | OrderCapacity_PRINCIPAL
  | OrderCapacity_RISKLESS_PRINCIPAL
  | OrderCapacity_AGENT_FOR_OTHER_MEMBER
  deriving stock (Show, Eq, Generic)

instance Validity OrderCapacity

instance IsField OrderCapacity where
  fieldTag Proxy = 528
  fieldIsData Proxy = False
  fieldToValue = \case
    OrderCapacity_AGENCY -> "A"
    OrderCapacity_PROPRIETARY -> "G"
    OrderCapacity_INDIVIDUAL -> "I"
    OrderCapacity_PRINCIPAL -> "P"
    OrderCapacity_RISKLESS_PRINCIPAL -> "R"
    OrderCapacity_AGENT_FOR_OTHER_MEMBER -> "W"
  fieldFromValue = \case
    "A" -> Right OrderCapacity_AGENCY
    "G" -> Right OrderCapacity_PROPRIETARY
    "I" -> Right OrderCapacity_INDIVIDUAL
    "P" -> Right OrderCapacity_PRINCIPAL
    "R" -> Right OrderCapacity_RISKLESS_PRINCIPAL
    "W" -> Right OrderCapacity_AGENT_FOR_OTHER_MEMBER
    v -> Left ("Unknown OrderCapacity: " <> show v)
