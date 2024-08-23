{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.Country where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 421, fieldName = "Country", fieldType = FieldTypeCountry, fieldValues = []}
newtype Country = Country {unCountry :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity Country

instance IsField Country where
  fieldTag Proxy = 421
  fieldIsData Proxy = False
  fieldToValue = toValue . unCountry
  fieldFromValue = fromValue >=> (prettyValidate . Country)
