{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.TradSesMode where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 339, fieldName = "TradSesMode", fieldType = FieldTypeInt, fieldValues = [FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "TESTING"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "SIMULATED"},FieldValueSpec {fieldValueEnum = "3", fieldValueDescription = "PRODUCTION"}]}
data TradSesMode
  = TradSesMode_TESTING
  | TradSesMode_SIMULATED
  | TradSesMode_PRODUCTION
  deriving stock (Show, Eq, Generic)

instance Validity TradSesMode

instance IsField TradSesMode where
  fieldTag Proxy = 339
  fieldIsData Proxy = False
  fieldToValue = \case
    TradSesMode_TESTING -> "1"
    TradSesMode_SIMULATED -> "2"
    TradSesMode_PRODUCTION -> "3"
  fieldFromValue = \case
    "1" -> Right TradSesMode_TESTING
    "2" -> Right TradSesMode_SIMULATED
    "3" -> Right TradSesMode_PRODUCTION
    v -> Left ("Unknown TradSesMode: " <> show v)
