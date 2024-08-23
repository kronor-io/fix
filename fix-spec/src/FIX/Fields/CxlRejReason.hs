{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.CxlRejReason where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 102, fieldName = "CxlRejReason", fieldType = FieldTypeInt, fieldValues = [FieldValueSpec {fieldValueEnum = "0", fieldValueDescription = "TOO_LATE_TO_CANCEL"},FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "UNKNOWN_ORDER"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "BROKER_CREDIT"},FieldValueSpec {fieldValueEnum = "3", fieldValueDescription = "ORDER_ALREADY_IN_PENDING_STATUS"},FieldValueSpec {fieldValueEnum = "4", fieldValueDescription = "UNABLE_TO_PROCESS_ORDER_MASS_CANCEL_REQUEST"},FieldValueSpec {fieldValueEnum = "5", fieldValueDescription = "ORIG_ORD_MOD_TIME"},FieldValueSpec {fieldValueEnum = "6", fieldValueDescription = "DUPLICATE_CL_ORD_ID"},FieldValueSpec {fieldValueEnum = "99", fieldValueDescription = "OTHER"}]}
data CxlRejReason
  = CxlRejReason_TOO_LATE_TO_CANCEL
  | CxlRejReason_UNKNOWN_ORDER
  | CxlRejReason_BROKER_CREDIT
  | CxlRejReason_ORDER_ALREADY_IN_PENDING_STATUS
  | CxlRejReason_UNABLE_TO_PROCESS_ORDER_MASS_CANCEL_REQUEST
  | CxlRejReason_ORIG_ORD_MOD_TIME
  | CxlRejReason_DUPLICATE_CL_ORD_ID
  | CxlRejReason_OTHER
  deriving stock (Show, Eq, Generic)

instance Validity CxlRejReason

instance IsField CxlRejReason where
  fieldTag Proxy = 102
  fieldIsData Proxy = False
  fieldToValue = \case
    CxlRejReason_TOO_LATE_TO_CANCEL -> "0"
    CxlRejReason_UNKNOWN_ORDER -> "1"
    CxlRejReason_BROKER_CREDIT -> "2"
    CxlRejReason_ORDER_ALREADY_IN_PENDING_STATUS -> "3"
    CxlRejReason_UNABLE_TO_PROCESS_ORDER_MASS_CANCEL_REQUEST -> "4"
    CxlRejReason_ORIG_ORD_MOD_TIME -> "5"
    CxlRejReason_DUPLICATE_CL_ORD_ID -> "6"
    CxlRejReason_OTHER -> "99"
  fieldFromValue = \case
    "0" -> Right CxlRejReason_TOO_LATE_TO_CANCEL
    "1" -> Right CxlRejReason_UNKNOWN_ORDER
    "2" -> Right CxlRejReason_BROKER_CREDIT
    "3" -> Right CxlRejReason_ORDER_ALREADY_IN_PENDING_STATUS
    "4" -> Right CxlRejReason_UNABLE_TO_PROCESS_ORDER_MASS_CANCEL_REQUEST
    "5" -> Right CxlRejReason_ORIG_ORD_MOD_TIME
    "6" -> Right CxlRejReason_DUPLICATE_CL_ORD_ID
    "99" -> Right CxlRejReason_OTHER
    v -> Left ("Unknown CxlRejReason: " <> show v)
