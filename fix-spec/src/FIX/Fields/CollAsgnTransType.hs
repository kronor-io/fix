{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.CollAsgnTransType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 903, fieldName = "CollAsgnTransType", fieldType = FieldTypeInt, fieldValues = [FieldValueSpec {fieldValueEnum = "0", fieldValueDescription = "NEW"},FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "REPLACE"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "CANCEL"},FieldValueSpec {fieldValueEnum = "3", fieldValueDescription = "RELEASE"},FieldValueSpec {fieldValueEnum = "4", fieldValueDescription = "REVERSE"}]}
data CollAsgnTransType
  = CollAsgnTransType_NEW
  | CollAsgnTransType_REPLACE
  | CollAsgnTransType_CANCEL
  | CollAsgnTransType_RELEASE
  | CollAsgnTransType_REVERSE
  deriving stock (Show, Eq, Generic)

instance Validity CollAsgnTransType

instance IsField CollAsgnTransType where
  fieldTag Proxy = 903
  fieldIsData Proxy = False
  fieldToValue = \case
    CollAsgnTransType_NEW -> "0"
    CollAsgnTransType_REPLACE -> "1"
    CollAsgnTransType_CANCEL -> "2"
    CollAsgnTransType_RELEASE -> "3"
    CollAsgnTransType_REVERSE -> "4"
  fieldFromValue = \case
    "0" -> Right CollAsgnTransType_NEW
    "1" -> Right CollAsgnTransType_REPLACE
    "2" -> Right CollAsgnTransType_CANCEL
    "3" -> Right CollAsgnTransType_RELEASE
    "4" -> Right CollAsgnTransType_REVERSE
    v -> Left ("Unknown CollAsgnTransType: " <> show v)
