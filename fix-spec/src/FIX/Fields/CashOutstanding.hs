{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.CashOutstanding where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 901, fieldName = "CashOutstanding", fieldType = FieldTypeAMT, fieldValues = []}
newtype CashOutstanding = CashOutstanding {unCashOutstanding :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity CashOutstanding

instance IsField CashOutstanding where
  fieldTag Proxy = 901
  fieldIsData Proxy = False
  fieldToValue = toValue . unCashOutstanding
  fieldFromValue = fromValue >=> (prettyValidate . CashOutstanding)
