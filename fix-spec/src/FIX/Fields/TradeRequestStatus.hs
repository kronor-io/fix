{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.TradeRequestStatus where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 750, fieldName = "TradeRequestStatus", fieldType = FieldTypeInt, fieldValues = [FieldValueSpec {fieldValueEnum = "0", fieldValueDescription = "ACCEPTED"},FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "COMPLETED"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "REJECTED"}]}
data TradeRequestStatus
  = TradeRequestStatus_ACCEPTED
  | TradeRequestStatus_COMPLETED
  | TradeRequestStatus_REJECTED
  deriving stock (Show, Eq, Generic)

instance Validity TradeRequestStatus

instance IsField TradeRequestStatus where
  fieldTag Proxy = 750
  fieldIsData Proxy = False
  fieldToValue = \case
    TradeRequestStatus_ACCEPTED -> "0"
    TradeRequestStatus_COMPLETED -> "1"
    TradeRequestStatus_REJECTED -> "2"
  fieldFromValue = \case
    "0" -> Right TradeRequestStatus_ACCEPTED
    "1" -> Right TradeRequestStatus_COMPLETED
    "2" -> Right TradeRequestStatus_REJECTED
    v -> Left ("Unknown TradeRequestStatus: " <> show v)
