{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.MsgDirection where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec {fieldNumber = 385, fieldName = "MsgDirection", fieldType = FieldTypeChar, fieldValues = [FieldValueSpec {fieldValueEnum = "S", fieldValueDescription = "SEND"},FieldValueSpec {fieldValueEnum = "R", fieldValueDescription = "RECEIVE"}]}
data MsgDirection
  = MsgDirection_SEND
  | MsgDirection_RECEIVE
  deriving stock (Show, Eq, Generic)

instance Validity MsgDirection

instance IsField MsgDirection where
  fieldTag Proxy = 385
  fieldIsData Proxy = False
  fieldToValue = \case
    MsgDirection_SEND -> "S"
    MsgDirection_RECEIVE -> "R"
  fieldFromValue = \case
    "S" -> Right MsgDirection_SEND
    "R" -> Right MsgDirection_RECEIVE
    v -> Left ("Unknown MsgDirection: " <> show v)