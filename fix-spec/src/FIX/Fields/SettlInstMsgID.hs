{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SettlInstMsgID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 777, fieldName = "SettlInstMsgID", fieldType = FieldTypeString, fieldValues = []}
newtype SettlInstMsgID = SettlInstMsgID {unSettlInstMsgID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity SettlInstMsgID

instance IsField SettlInstMsgID where
  fieldTag Proxy = 777
  fieldIsData Proxy = False
  fieldToValue = toValue . unSettlInstMsgID
  fieldFromValue = fromValue >=> (prettyValidate . SettlInstMsgID)
