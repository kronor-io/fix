{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.MinOfferSize where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 648, fieldName = "MinOfferSize", fieldType = FieldTypeQTY, fieldValues = []}
newtype MinOfferSize = MinOfferSize {unMinOfferSize :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity MinOfferSize

instance IsField MinOfferSize where
  fieldTag Proxy = 648
  fieldIsData Proxy = False
  fieldToValue = toValue . unMinOfferSize
  fieldFromValue = fromValue >=> (prettyValidate . MinOfferSize)
