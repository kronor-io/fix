{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.MiscFeeBasis where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 891, fieldName = "MiscFeeBasis", fieldType = FieldTypeInt, fieldValues = [FieldValueSpec {fieldValueEnum = "0", fieldValueDescription = "ABSOLUTE"},FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "PER_UNIT"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "PERCENTAGE"}]}
data MiscFeeBasis
  = MiscFeeBasis_ABSOLUTE
  | MiscFeeBasis_PER_UNIT
  | MiscFeeBasis_PERCENTAGE
  deriving stock (Show, Eq, Generic)

instance Validity MiscFeeBasis

instance IsField MiscFeeBasis where
  fieldTag Proxy = 891
  fieldIsData Proxy = False
  fieldToValue = \case
    MiscFeeBasis_ABSOLUTE -> "0"
    MiscFeeBasis_PER_UNIT -> "1"
    MiscFeeBasis_PERCENTAGE -> "2"
  fieldFromValue = \case
    "0" -> Right MiscFeeBasis_ABSOLUTE
    "1" -> Right MiscFeeBasis_PER_UNIT
    "2" -> Right MiscFeeBasis_PERCENTAGE
    v -> Left ("Unknown MiscFeeBasis: " <> show v)
