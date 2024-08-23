{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.PegLimitType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 837, fieldName = "PegLimitType", fieldType = FieldTypeInt, fieldValues = [FieldValueSpec {fieldValueEnum = "0", fieldValueDescription = "OR_BETTER"},FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "STRICT"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "OR_WORSE"}]}
data PegLimitType
  = PegLimitType_OR_BETTER
  | PegLimitType_STRICT
  | PegLimitType_OR_WORSE
  deriving stock (Show, Eq, Generic)

instance Validity PegLimitType

instance IsField PegLimitType where
  fieldTag Proxy = 837
  fieldIsData Proxy = False
  fieldToValue = \case
    PegLimitType_OR_BETTER -> "0"
    PegLimitType_STRICT -> "1"
    PegLimitType_OR_WORSE -> "2"
  fieldFromValue = \case
    "0" -> Right PegLimitType_OR_BETTER
    "1" -> Right PegLimitType_STRICT
    "2" -> Right PegLimitType_OR_WORSE
    v -> Left ("Unknown PegLimitType: " <> show v)
