{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SettlPriceType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 731, fieldName = "SettlPriceType", fieldType = FieldTypeInt, fieldValues = [FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "FINAL"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "THEORETICAL"}]}
data SettlPriceType
  = SettlPriceType_FINAL
  | SettlPriceType_THEORETICAL
  deriving stock (Show, Eq, Generic)

instance Validity SettlPriceType

instance IsField SettlPriceType where
  fieldTag Proxy = 731
  fieldIsData Proxy = False
  fieldToValue = \case
    SettlPriceType_FINAL -> "1"
    SettlPriceType_THEORETICAL -> "2"
  fieldFromValue = \case
    "1" -> Right SettlPriceType_FINAL
    "2" -> Right SettlPriceType_THEORETICAL
    v -> Left ("Unknown SettlPriceType: " <> show v)
