{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SecurityResponseType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 323, fieldName = "SecurityResponseType", fieldType = FieldTypeInt, fieldValues = [FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "ACCEPT_AS_IS"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "ACCEPT_WITH_REVISIONS"},FieldValueSpec {fieldValueEnum = "5", fieldValueDescription = "REJECT_SECURITY_PROPOSAL"},FieldValueSpec {fieldValueEnum = "6", fieldValueDescription = "CANNOT_MATCH_SELECTION_CRITERIA"}]}
data SecurityResponseType
  = SecurityResponseType_ACCEPT_AS_IS
  | SecurityResponseType_ACCEPT_WITH_REVISIONS
  | SecurityResponseType_REJECT_SECURITY_PROPOSAL
  | SecurityResponseType_CANNOT_MATCH_SELECTION_CRITERIA
  deriving stock (Show, Eq, Generic)

instance Validity SecurityResponseType

instance IsField SecurityResponseType where
  fieldTag Proxy = 323
  fieldIsData Proxy = False
  fieldToValue = \case
    SecurityResponseType_ACCEPT_AS_IS -> "1"
    SecurityResponseType_ACCEPT_WITH_REVISIONS -> "2"
    SecurityResponseType_REJECT_SECURITY_PROPOSAL -> "5"
    SecurityResponseType_CANNOT_MATCH_SELECTION_CRITERIA -> "6"
  fieldFromValue = \case
    "1" -> Right SecurityResponseType_ACCEPT_AS_IS
    "2" -> Right SecurityResponseType_ACCEPT_WITH_REVISIONS
    "5" -> Right SecurityResponseType_REJECT_SECURITY_PROPOSAL
    "6" -> Right SecurityResponseType_CANNOT_MATCH_SELECTION_CRITERIA
    v -> Left ("Unknown SecurityResponseType: " <> show v)
