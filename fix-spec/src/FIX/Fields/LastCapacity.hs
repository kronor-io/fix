{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LastCapacity where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 29, fieldName = "LastCapacity", fieldType = FieldTypeChar, fieldValues = [FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "AGENT"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "CROSS_AS_AGENT"},FieldValueSpec {fieldValueEnum = "3", fieldValueDescription = "CROSS_AS_PRINCIPAL"},FieldValueSpec {fieldValueEnum = "4", fieldValueDescription = "PRINCIPAL"}]}
data LastCapacity
  = LastCapacity_AGENT
  | LastCapacity_CROSS_AS_AGENT
  | LastCapacity_CROSS_AS_PRINCIPAL
  | LastCapacity_PRINCIPAL
  deriving stock (Show, Eq, Generic)

instance Validity LastCapacity

instance IsField LastCapacity where
  fieldTag Proxy = 29
  fieldIsData Proxy = False
  fieldToValue = \case
    LastCapacity_AGENT -> "1"
    LastCapacity_CROSS_AS_AGENT -> "2"
    LastCapacity_CROSS_AS_PRINCIPAL -> "3"
    LastCapacity_PRINCIPAL -> "4"
  fieldFromValue = \case
    "1" -> Right LastCapacity_AGENT
    "2" -> Right LastCapacity_CROSS_AS_AGENT
    "3" -> Right LastCapacity_CROSS_AS_PRINCIPAL
    "4" -> Right LastCapacity_PRINCIPAL
    v -> Left ("Unknown LastCapacity: " <> show v)
