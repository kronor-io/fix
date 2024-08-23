{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.RegistDtls where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 509, fieldName = "RegistDtls", fieldType = FieldTypeString, fieldValues = []}
newtype RegistDtls = RegistDtls {unRegistDtls :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity RegistDtls

instance IsField RegistDtls where
  fieldTag Proxy = 509
  fieldIsData Proxy = False
  fieldToValue = toValue . unRegistDtls
  fieldFromValue = fromValue >=> (prettyValidate . RegistDtls)
