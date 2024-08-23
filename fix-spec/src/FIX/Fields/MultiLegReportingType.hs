{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.MultiLegReportingType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 442, fieldName = "MultiLegReportingType", fieldType = FieldTypeChar, fieldValues = [FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "SINGLE_SECURITY"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "INDIVIDUAL_LEG_OF_A_MULTI_LEG_SECURITY"},FieldValueSpec {fieldValueEnum = "3", fieldValueDescription = "MULTI_LEG_SECURITY"}]}
data MultiLegReportingType
  = MultiLegReportingType_SINGLE_SECURITY
  | MultiLegReportingType_INDIVIDUAL_LEG_OF_A_MULTI_LEG_SECURITY
  | MultiLegReportingType_MULTI_LEG_SECURITY
  deriving stock (Show, Eq, Generic)

instance Validity MultiLegReportingType

instance IsField MultiLegReportingType where
  fieldTag Proxy = 442
  fieldIsData Proxy = False
  fieldToValue = \case
    MultiLegReportingType_SINGLE_SECURITY -> "1"
    MultiLegReportingType_INDIVIDUAL_LEG_OF_A_MULTI_LEG_SECURITY -> "2"
    MultiLegReportingType_MULTI_LEG_SECURITY -> "3"
  fieldFromValue = \case
    "1" -> Right MultiLegReportingType_SINGLE_SECURITY
    "2" -> Right MultiLegReportingType_INDIVIDUAL_LEG_OF_A_MULTI_LEG_SECURITY
    "3" -> Right MultiLegReportingType_MULTI_LEG_SECURITY
    v -> Left ("Unknown MultiLegReportingType: " <> show v)
