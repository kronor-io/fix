{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SettlPartyRole where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 784, fieldName = "SettlPartyRole", fieldType = FieldTypeInt, fieldValues = []}
newtype SettlPartyRole = SettlPartyRole {unSettlPartyRole :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity SettlPartyRole

instance IsField SettlPartyRole where
  fieldTag Proxy = 784
  fieldIsData Proxy = False
  fieldToValue = toValue . unSettlPartyRole
  fieldFromValue = fromValue >=> (prettyValidate . SettlPartyRole)
