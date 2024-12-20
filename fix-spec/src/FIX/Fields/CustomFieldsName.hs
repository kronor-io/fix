{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.CustomFieldsName where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 7547
--   , fieldName = "CustomFieldsName"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype CustomFieldsName = CustomFieldsName {unCustomFieldsName :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity CustomFieldsName

instance IsField CustomFieldsName where
  fieldTag Proxy = 7547
  fieldIsData Proxy = False
  fieldToValue = toValue . unCustomFieldsName
  fieldFromValue = fromValue >=> (prettyValidate . CustomFieldsName)
