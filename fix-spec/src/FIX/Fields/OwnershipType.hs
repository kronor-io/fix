{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.OwnershipType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 517, fieldName = "OwnershipType", fieldType = FieldTypeChar, fieldValues = [FieldValueSpec {fieldValueEnum = "J", fieldValueDescription = "JOINT_INVESTORS"},FieldValueSpec {fieldValueEnum = "T", fieldValueDescription = "TENANTS_IN_COMMON"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "JOINT_TRUSTEES"}]}
data OwnershipType
  = OwnershipType_JOINT_INVESTORS
  | OwnershipType_TENANTS_IN_COMMON
  | OwnershipType_JOINT_TRUSTEES
  deriving stock (Show, Eq, Generic)

instance Validity OwnershipType

instance IsField OwnershipType where
  fieldTag Proxy = 517
  fieldIsData Proxy = False
  fieldToValue = \case
    OwnershipType_JOINT_INVESTORS -> "J"
    OwnershipType_TENANTS_IN_COMMON -> "T"
    OwnershipType_JOINT_TRUSTEES -> "2"
  fieldFromValue = \case
    "J" -> Right OwnershipType_JOINT_INVESTORS
    "T" -> Right OwnershipType_TENANTS_IN_COMMON
    "2" -> Right OwnershipType_JOINT_TRUSTEES
    v -> Left ("Unknown OwnershipType: " <> show v)
