{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SettlSessSubID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 717, fieldName = "SettlSessSubID", fieldType = FieldTypeString, fieldValues = []}
newtype SettlSessSubID = SettlSessSubID {unSettlSessSubID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity SettlSessSubID

instance IsField SettlSessSubID where
  fieldTag Proxy = 717
  fieldIsData Proxy = False
  fieldToValue = toValue . unSettlSessSubID
  fieldFromValue = fromValue >=> (prettyValidate . SettlSessSubID)
