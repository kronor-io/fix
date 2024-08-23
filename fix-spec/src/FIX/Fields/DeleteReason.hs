{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.DeleteReason where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 285, fieldName = "DeleteReason", fieldType = FieldTypeChar, fieldValues = [FieldValueSpec {fieldValueEnum = "0", fieldValueDescription = "CANCELLATION"},FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "ERROR"}]}
data DeleteReason
  = DeleteReason_CANCELLATION
  | DeleteReason_ERROR
  deriving stock (Show, Eq, Generic)

instance Validity DeleteReason

instance IsField DeleteReason where
  fieldTag Proxy = 285
  fieldIsData Proxy = False
  fieldToValue = \case
    DeleteReason_CANCELLATION -> "0"
    DeleteReason_ERROR -> "1"
  fieldFromValue = \case
    "0" -> Right DeleteReason_CANCELLATION
    "1" -> Right DeleteReason_ERROR
    v -> Left ("Unknown DeleteReason: " <> show v)
