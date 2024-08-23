{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.AllocReportRefID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 795, fieldName = "AllocReportRefID", fieldType = FieldTypeString, fieldValues = []}
newtype AllocReportRefID = AllocReportRefID {unAllocReportRefID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity AllocReportRefID

instance IsField AllocReportRefID where
  fieldTag Proxy = 795
  fieldIsData Proxy = False
  fieldToValue = toValue . unAllocReportRefID
  fieldFromValue = fromValue >=> (prettyValidate . AllocReportRefID)
