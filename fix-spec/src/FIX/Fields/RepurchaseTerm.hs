{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.RepurchaseTerm where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 226, fieldName = "RepurchaseTerm", fieldType = FieldTypeInt, fieldValues = []}
newtype RepurchaseTerm = RepurchaseTerm {unRepurchaseTerm :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity RepurchaseTerm

instance IsField RepurchaseTerm where
  fieldTag Proxy = 226
  fieldIsData Proxy = False
  fieldToValue = toValue . unRepurchaseTerm
  fieldFromValue = fromValue >=> (prettyValidate . RepurchaseTerm)
