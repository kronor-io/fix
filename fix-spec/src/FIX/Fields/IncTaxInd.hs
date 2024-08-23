{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.IncTaxInd where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 416, fieldName = "IncTaxInd", fieldType = FieldTypeInt, fieldValues = [FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "NET"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "GROSS"}]}
data IncTaxInd
  = IncTaxInd_NET
  | IncTaxInd_GROSS
  deriving stock (Show, Eq, Generic)

instance Validity IncTaxInd

instance IsField IncTaxInd where
  fieldTag Proxy = 416
  fieldIsData Proxy = False
  fieldToValue = \case
    IncTaxInd_NET -> "1"
    IncTaxInd_GROSS -> "2"
  fieldFromValue = \case
    "1" -> Right IncTaxInd_NET
    "2" -> Right IncTaxInd_GROSS
    v -> Left ("Unknown IncTaxInd: " <> show v)
