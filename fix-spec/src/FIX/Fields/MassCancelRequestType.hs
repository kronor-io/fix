{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.MassCancelRequestType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 530, fieldName = "MassCancelRequestType", fieldType = FieldTypeChar, fieldValues = [FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "CANCEL_ORDERS_FOR_A_SECURITY"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "CANCEL_ORDERS_FOR_AN_UNDERLYING_SECURITY"},FieldValueSpec {fieldValueEnum = "3", fieldValueDescription = "CANCEL_ORDERS_FOR_A_PRODUCT"},FieldValueSpec {fieldValueEnum = "4", fieldValueDescription = "CANCEL_ORDERS_FOR_ACFI_CODE"},FieldValueSpec {fieldValueEnum = "5", fieldValueDescription = "CANCEL_ORDERS_FOR_A_SECURITY_TYPE"},FieldValueSpec {fieldValueEnum = "6", fieldValueDescription = "CANCEL_ORDERS_FOR_A_TRADING_SESSION"},FieldValueSpec {fieldValueEnum = "7", fieldValueDescription = "CANCEL_ALL_ORDERS"}]}
data MassCancelRequestType
  = MassCancelRequestType_CANCEL_ORDERS_FOR_A_SECURITY
  | MassCancelRequestType_CANCEL_ORDERS_FOR_AN_UNDERLYING_SECURITY
  | MassCancelRequestType_CANCEL_ORDERS_FOR_A_PRODUCT
  | MassCancelRequestType_CANCEL_ORDERS_FOR_ACFI_CODE
  | MassCancelRequestType_CANCEL_ORDERS_FOR_A_SECURITY_TYPE
  | MassCancelRequestType_CANCEL_ORDERS_FOR_A_TRADING_SESSION
  | MassCancelRequestType_CANCEL_ALL_ORDERS
  deriving stock (Show, Eq, Generic)

instance Validity MassCancelRequestType

instance IsField MassCancelRequestType where
  fieldTag Proxy = 530
  fieldIsData Proxy = False
  fieldToValue = \case
    MassCancelRequestType_CANCEL_ORDERS_FOR_A_SECURITY -> "1"
    MassCancelRequestType_CANCEL_ORDERS_FOR_AN_UNDERLYING_SECURITY -> "2"
    MassCancelRequestType_CANCEL_ORDERS_FOR_A_PRODUCT -> "3"
    MassCancelRequestType_CANCEL_ORDERS_FOR_ACFI_CODE -> "4"
    MassCancelRequestType_CANCEL_ORDERS_FOR_A_SECURITY_TYPE -> "5"
    MassCancelRequestType_CANCEL_ORDERS_FOR_A_TRADING_SESSION -> "6"
    MassCancelRequestType_CANCEL_ALL_ORDERS -> "7"
  fieldFromValue = \case
    "1" -> Right MassCancelRequestType_CANCEL_ORDERS_FOR_A_SECURITY
    "2" -> Right MassCancelRequestType_CANCEL_ORDERS_FOR_AN_UNDERLYING_SECURITY
    "3" -> Right MassCancelRequestType_CANCEL_ORDERS_FOR_A_PRODUCT
    "4" -> Right MassCancelRequestType_CANCEL_ORDERS_FOR_ACFI_CODE
    "5" -> Right MassCancelRequestType_CANCEL_ORDERS_FOR_A_SECURITY_TYPE
    "6" -> Right MassCancelRequestType_CANCEL_ORDERS_FOR_A_TRADING_SESSION
    "7" -> Right MassCancelRequestType_CANCEL_ALL_ORDERS
    v -> Left ("Unknown MassCancelRequestType: " <> show v)
