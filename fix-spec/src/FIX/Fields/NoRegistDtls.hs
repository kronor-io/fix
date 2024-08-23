{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.NoRegistDtls where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 473, fieldName = "NoRegistDtls", fieldType = FieldTypeNumInGroup, fieldValues = []}
newtype NoRegistDtls = NoRegistDtls {unNoRegistDtls :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity NoRegistDtls

instance IsField NoRegistDtls where
  fieldTag Proxy = 473
  fieldIsData Proxy = False
  fieldToValue = toValue . unNoRegistDtls
  fieldFromValue = fromValue >=> (prettyValidate . NoRegistDtls)
