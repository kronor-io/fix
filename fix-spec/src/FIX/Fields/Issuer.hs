{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.Issuer where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 106, fieldName = "Issuer", fieldType = FieldTypeString, fieldValues = []}
newtype Issuer = Issuer {unIssuer :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity Issuer

instance IsField Issuer where
  fieldTag Proxy = 106
  fieldIsData Proxy = False
  fieldToValue = toValue . unIssuer
  fieldFromValue = fromValue >=> (prettyValidate . Issuer)
