{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.NetworkStatusResponseType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 937, fieldName = "NetworkStatusResponseType", fieldType = FieldTypeInt, fieldValues = [FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "FULL"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "INCREMENTAL_UPDATE"}]}
data NetworkStatusResponseType
  = NetworkStatusResponseType_FULL
  | NetworkStatusResponseType_INCREMENTAL_UPDATE
  deriving stock (Show, Eq, Generic)

instance Validity NetworkStatusResponseType

instance IsField NetworkStatusResponseType where
  fieldTag Proxy = 937
  fieldIsData Proxy = False
  fieldToValue = \case
    NetworkStatusResponseType_FULL -> "1"
    NetworkStatusResponseType_INCREMENTAL_UPDATE -> "2"
  fieldFromValue = \case
    "1" -> Right NetworkStatusResponseType_FULL
    "2" -> Right NetworkStatusResponseType_INCREMENTAL_UPDATE
    v -> Left ("Unknown NetworkStatusResponseType: " <> show v)
