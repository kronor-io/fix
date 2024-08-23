{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.AllocAvgPx where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 153, fieldName = "AllocAvgPx", fieldType = FieldTypePrice, fieldValues = []}
newtype AllocAvgPx = AllocAvgPx {unAllocAvgPx :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity AllocAvgPx

instance IsField AllocAvgPx where
  fieldTag Proxy = 153
  fieldIsData Proxy = False
  fieldToValue = toValue . unAllocAvgPx
  fieldFromValue = fromValue >=> (prettyValidate . AllocAvgPx)
