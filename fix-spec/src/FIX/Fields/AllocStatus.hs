{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.AllocStatus where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 87, fieldName = "AllocStatus", fieldType = FieldTypeInt, fieldValues = [FieldValueSpec {fieldValueEnum = "0", fieldValueDescription = "ACCEPTED"},FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "BLOCK_LEVEL_REJECT"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "ACCOUNT_LEVEL_REJECT"},FieldValueSpec {fieldValueEnum = "3", fieldValueDescription = "RECEIVED"},FieldValueSpec {fieldValueEnum = "4", fieldValueDescription = "INCOMPLETE"},FieldValueSpec {fieldValueEnum = "5", fieldValueDescription = "REJECTED_BY_INTERMEDIARY"}]}
data AllocStatus
  = AllocStatus_ACCEPTED
  | AllocStatus_BLOCK_LEVEL_REJECT
  | AllocStatus_ACCOUNT_LEVEL_REJECT
  | AllocStatus_RECEIVED
  | AllocStatus_INCOMPLETE
  | AllocStatus_REJECTED_BY_INTERMEDIARY
  deriving stock (Show, Eq, Generic)

instance Validity AllocStatus

instance IsField AllocStatus where
  fieldTag Proxy = 87
  fieldIsData Proxy = False
  fieldToValue = \case
    AllocStatus_ACCEPTED -> "0"
    AllocStatus_BLOCK_LEVEL_REJECT -> "1"
    AllocStatus_ACCOUNT_LEVEL_REJECT -> "2"
    AllocStatus_RECEIVED -> "3"
    AllocStatus_INCOMPLETE -> "4"
    AllocStatus_REJECTED_BY_INTERMEDIARY -> "5"
  fieldFromValue = \case
    "0" -> Right AllocStatus_ACCEPTED
    "1" -> Right AllocStatus_BLOCK_LEVEL_REJECT
    "2" -> Right AllocStatus_ACCOUNT_LEVEL_REJECT
    "3" -> Right AllocStatus_RECEIVED
    "4" -> Right AllocStatus_INCOMPLETE
    "5" -> Right AllocStatus_REJECTED_BY_INTERMEDIARY
    v -> Left ("Unknown AllocStatus: " <> show v)
