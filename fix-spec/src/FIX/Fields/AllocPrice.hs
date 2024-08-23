{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.AllocPrice where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 366, fieldName = "AllocPrice", fieldType = FieldTypePrice, fieldValues = []}
newtype AllocPrice = AllocPrice {unAllocPrice :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity AllocPrice

instance IsField AllocPrice where
  fieldTag Proxy = 366
  fieldIsData Proxy = False
  fieldToValue = toValue . unAllocPrice
  fieldFromValue = fromValue >=> (prettyValidate . AllocPrice)
