{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegAllocAcctIDSource where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 674, fieldName = "LegAllocAcctIDSource", fieldType = FieldTypeString, fieldValues = []}
newtype LegAllocAcctIDSource = LegAllocAcctIDSource {unLegAllocAcctIDSource :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity LegAllocAcctIDSource

instance IsField LegAllocAcctIDSource where
  fieldTag Proxy = 674
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegAllocAcctIDSource
  fieldFromValue = fromValue >=> (prettyValidate . LegAllocAcctIDSource)
