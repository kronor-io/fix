{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.AllocQty where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 80, fieldName = "AllocQty", fieldType = FieldTypeQTY, fieldValues = []}
newtype AllocQty = AllocQty {unAllocQty :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity AllocQty

instance IsField AllocQty where
  fieldTag Proxy = 80
  fieldIsData Proxy = False
  fieldToValue = toValue . unAllocQty
  fieldFromValue = fromValue >=> (prettyValidate . AllocQty)
