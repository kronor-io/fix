{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.PegRoundDirection where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 838, fieldName = "PegRoundDirection", fieldType = FieldTypeInt, fieldValues = [FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "MORE_AGGRESSIVE"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "MORE_PASSIVE"}]}
data PegRoundDirection
  = PegRoundDirection_MORE_AGGRESSIVE
  | PegRoundDirection_MORE_PASSIVE
  deriving stock (Show, Eq, Generic)

instance Validity PegRoundDirection

instance IsField PegRoundDirection where
  fieldTag Proxy = 838
  fieldIsData Proxy = False
  fieldToValue = \case
    PegRoundDirection_MORE_AGGRESSIVE -> "1"
    PegRoundDirection_MORE_PASSIVE -> "2"
  fieldFromValue = \case
    "1" -> Right PegRoundDirection_MORE_AGGRESSIVE
    "2" -> Right PegRoundDirection_MORE_PASSIVE
    v -> Left ("Unknown PegRoundDirection: " <> show v)
