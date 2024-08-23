{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegSecurityType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 609, fieldName = "LegSecurityType", fieldType = FieldTypeString, fieldValues = []}
newtype LegSecurityType = LegSecurityType {unLegSecurityType :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity LegSecurityType

instance IsField LegSecurityType where
  fieldTag Proxy = 609
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegSecurityType
  fieldFromValue = fromValue >=> (prettyValidate . LegSecurityType)
