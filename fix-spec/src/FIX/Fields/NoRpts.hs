{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.NoRpts where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 82, fieldName = "NoRpts", fieldType = FieldTypeInt, fieldValues = []}
newtype NoRpts = NoRpts {unNoRpts :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity NoRpts

instance IsField NoRpts where
  fieldTag Proxy = 82
  fieldIsData Proxy = False
  fieldToValue = toValue . unNoRpts
  fieldFromValue = fromValue >=> (prettyValidate . NoRpts)
