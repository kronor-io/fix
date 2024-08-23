{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.CashDistribAgentCode where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 499, fieldName = "CashDistribAgentCode", fieldType = FieldTypeString, fieldValues = []}
newtype CashDistribAgentCode = CashDistribAgentCode {unCashDistribAgentCode :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity CashDistribAgentCode

instance IsField CashDistribAgentCode where
  fieldTag Proxy = 499
  fieldIsData Proxy = False
  fieldToValue = toValue . unCashDistribAgentCode
  fieldFromValue = fromValue >=> (prettyValidate . CashDistribAgentCode)
