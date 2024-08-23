{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.UnderlyingRepurchaseTerm where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 244, fieldName = "UnderlyingRepurchaseTerm", fieldType = FieldTypeInt, fieldValues = []}
newtype UnderlyingRepurchaseTerm = UnderlyingRepurchaseTerm {unUnderlyingRepurchaseTerm :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity UnderlyingRepurchaseTerm

instance IsField UnderlyingRepurchaseTerm where
  fieldTag Proxy = 244
  fieldIsData Proxy = False
  fieldToValue = toValue . unUnderlyingRepurchaseTerm
  fieldFromValue = fromValue >=> (prettyValidate . UnderlyingRepurchaseTerm)
