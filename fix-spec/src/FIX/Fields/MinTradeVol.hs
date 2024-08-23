{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.MinTradeVol where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 562, fieldName = "MinTradeVol", fieldType = FieldTypeQTY, fieldValues = []}
newtype MinTradeVol = MinTradeVol {unMinTradeVol :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity MinTradeVol

instance IsField MinTradeVol where
  fieldTag Proxy = 562
  fieldIsData Proxy = False
  fieldToValue = toValue . unMinTradeVol
  fieldFromValue = fromValue >=> (prettyValidate . MinTradeVol)
