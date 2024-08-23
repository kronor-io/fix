{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.PegOffsetType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 836, fieldName = "PegOffsetType", fieldType = FieldTypeInt, fieldValues = [FieldValueSpec {fieldValueEnum = "0", fieldValueDescription = "PRICE"},FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "BASIS_POINTS"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "TICKS"},FieldValueSpec {fieldValueEnum = "3", fieldValueDescription = "PRICE_TIER"}]}
data PegOffsetType
  = PegOffsetType_PRICE
  | PegOffsetType_BASIS_POINTS
  | PegOffsetType_TICKS
  | PegOffsetType_PRICE_TIER
  deriving stock (Show, Eq, Generic)

instance Validity PegOffsetType

instance IsField PegOffsetType where
  fieldTag Proxy = 836
  fieldIsData Proxy = False
  fieldToValue = \case
    PegOffsetType_PRICE -> "0"
    PegOffsetType_BASIS_POINTS -> "1"
    PegOffsetType_TICKS -> "2"
    PegOffsetType_PRICE_TIER -> "3"
  fieldFromValue = \case
    "0" -> Right PegOffsetType_PRICE
    "1" -> Right PegOffsetType_BASIS_POINTS
    "2" -> Right PegOffsetType_TICKS
    "3" -> Right PegOffsetType_PRICE_TIER
    v -> Left ("Unknown PegOffsetType: " <> show v)
