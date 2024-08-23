{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LocateReqd where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 114, fieldName = "LocateReqd", fieldType = FieldTypeBoolean, fieldValues = [FieldValueSpec {fieldValueEnum = "Y", fieldValueDescription = "YES"},FieldValueSpec {fieldValueEnum = "N", fieldValueDescription = "NO"}]}
data LocateReqd
  = LocateReqd_YES
  | LocateReqd_NO
  deriving stock (Show, Eq, Generic)

instance Validity LocateReqd

instance IsField LocateReqd where
  fieldTag Proxy = 114
  fieldIsData Proxy = False
  fieldToValue = \case
    LocateReqd_YES -> "Y"
    LocateReqd_NO -> "N"
  fieldFromValue = \case
    "Y" -> Right LocateReqd_YES
    "N" -> Right LocateReqd_NO
    v -> Left ("Unknown LocateReqd: " <> show v)
