{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.AggregatedBook where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 266, fieldName = "AggregatedBook", fieldType = FieldTypeBoolean, fieldValues = [FieldValueSpec {fieldValueEnum = "Y", fieldValueDescription = "YES"},FieldValueSpec {fieldValueEnum = "N", fieldValueDescription = "NO"}]}
data AggregatedBook
  = AggregatedBook_YES
  | AggregatedBook_NO
  deriving stock (Show, Eq, Generic)

instance Validity AggregatedBook

instance IsField AggregatedBook where
  fieldTag Proxy = 266
  fieldIsData Proxy = False
  fieldToValue = \case
    AggregatedBook_YES -> "Y"
    AggregatedBook_NO -> "N"
  fieldFromValue = \case
    "Y" -> Right AggregatedBook_YES
    "N" -> Right AggregatedBook_NO
    v -> Left ("Unknown AggregatedBook: " <> show v)
