{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.UnderlyingEndPrice where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 883, fieldName = "UnderlyingEndPrice", fieldType = FieldTypePrice, fieldValues = []}
newtype UnderlyingEndPrice = UnderlyingEndPrice {unUnderlyingEndPrice :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity UnderlyingEndPrice

instance IsField UnderlyingEndPrice where
  fieldTag Proxy = 883
  fieldIsData Proxy = False
  fieldToValue = toValue . unUnderlyingEndPrice
  fieldFromValue = fromValue >=> (prettyValidate . UnderlyingEndPrice)
