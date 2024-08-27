{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.TestMessageIndicator where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec {fieldNumber = 464, fieldName = "TestMessageIndicator", fieldType = FieldTypeBoolean, fieldValues = [FieldValueSpec {fieldValueEnum = "Y", fieldValueDescription = "YES"},FieldValueSpec {fieldValueEnum = "N", fieldValueDescription = "NO"}]}
data TestMessageIndicator
  = TestMessageIndicator_YES
  | TestMessageIndicator_NO
  deriving stock (Show, Eq, Generic)

instance Validity TestMessageIndicator

instance IsField TestMessageIndicator where
  fieldTag Proxy = 464
  fieldIsData Proxy = False
  fieldToValue = \case
    TestMessageIndicator_YES -> "Y"
    TestMessageIndicator_NO -> "N"
  fieldFromValue = \case
    "Y" -> Right TestMessageIndicator_YES
    "N" -> Right TestMessageIndicator_NO
    v -> Left ("Unknown TestMessageIndicator: " <> show v)
