{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.AgreementDesc where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 913, fieldName = "AgreementDesc", fieldType = FieldTypeString, fieldValues = []}
newtype AgreementDesc = AgreementDesc {unAgreementDesc :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity AgreementDesc

instance IsField AgreementDesc where
  fieldTag Proxy = 913
  fieldIsData Proxy = False
  fieldToValue = toValue . unAgreementDesc
  fieldFromValue = fromValue >=> (prettyValidate . AgreementDesc)
