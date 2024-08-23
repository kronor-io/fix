{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SettlInstMode where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 160, fieldName = "SettlInstMode", fieldType = FieldTypeChar, fieldValues = [FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "STANDING_INSTRUCTIONS_PROVIDED"},FieldValueSpec {fieldValueEnum = "4", fieldValueDescription = "SPECIFIC_ORDER_FOR_A_SINGLE_ACCOUNT"},FieldValueSpec {fieldValueEnum = "5", fieldValueDescription = "REQUEST_REJECT"}]}
data SettlInstMode
  = SettlInstMode_STANDING_INSTRUCTIONS_PROVIDED
  | SettlInstMode_SPECIFIC_ORDER_FOR_A_SINGLE_ACCOUNT
  | SettlInstMode_REQUEST_REJECT
  deriving stock (Show, Eq, Generic)

instance Validity SettlInstMode

instance IsField SettlInstMode where
  fieldTag Proxy = 160
  fieldIsData Proxy = False
  fieldToValue = \case
    SettlInstMode_STANDING_INSTRUCTIONS_PROVIDED -> "1"
    SettlInstMode_SPECIFIC_ORDER_FOR_A_SINGLE_ACCOUNT -> "4"
    SettlInstMode_REQUEST_REJECT -> "5"
  fieldFromValue = \case
    "1" -> Right SettlInstMode_STANDING_INSTRUCTIONS_PROVIDED
    "4" -> Right SettlInstMode_SPECIFIC_ORDER_FOR_A_SINGLE_ACCOUNT
    "5" -> Right SettlInstMode_REQUEST_REJECT
    v -> Left ("Unknown SettlInstMode: " <> show v)
