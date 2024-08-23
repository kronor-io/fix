{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SettlInstReqID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 791, fieldName = "SettlInstReqID", fieldType = FieldTypeString, fieldValues = []}
newtype SettlInstReqID = SettlInstReqID {unSettlInstReqID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity SettlInstReqID

instance IsField SettlInstReqID where
  fieldTag Proxy = 791
  fieldIsData Proxy = False
  fieldToValue = toValue . unSettlInstReqID
  fieldFromValue = fromValue >=> (prettyValidate . SettlInstReqID)
