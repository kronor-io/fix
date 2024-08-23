{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.TradeReportRefID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 572, fieldName = "TradeReportRefID", fieldType = FieldTypeString, fieldValues = []}
newtype TradeReportRefID = TradeReportRefID {unTradeReportRefID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity TradeReportRefID

instance IsField TradeReportRefID where
  fieldTag Proxy = 572
  fieldIsData Proxy = False
  fieldToValue = toValue . unTradeReportRefID
  fieldFromValue = fromValue >=> (prettyValidate . TradeReportRefID)
