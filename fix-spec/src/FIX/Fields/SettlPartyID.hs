{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SettlPartyID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 782, fieldName = "SettlPartyID", fieldType = FieldTypeString, fieldValues = []}
newtype SettlPartyID = SettlPartyID {unSettlPartyID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity SettlPartyID

instance IsField SettlPartyID where
  fieldTag Proxy = 782
  fieldIsData Proxy = False
  fieldToValue = toValue . unSettlPartyID
  fieldFromValue = fromValue >=> (prettyValidate . SettlPartyID)
