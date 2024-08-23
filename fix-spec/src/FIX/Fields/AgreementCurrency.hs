{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.AgreementCurrency where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 918, fieldName = "AgreementCurrency", fieldType = FieldTypeCurrency, fieldValues = []}
newtype AgreementCurrency = AgreementCurrency {unAgreementCurrency :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity AgreementCurrency

instance IsField AgreementCurrency where
  fieldTag Proxy = 918
  fieldIsData Proxy = False
  fieldToValue = toValue . unAgreementCurrency
  fieldFromValue = fromValue >=> (prettyValidate . AgreementCurrency)
