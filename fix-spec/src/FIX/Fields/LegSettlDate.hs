{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegSettlDate where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 588, fieldName = "LegSettlDate", fieldType = FieldTypeLocalMktDate, fieldValues = []}
newtype LegSettlDate = LegSettlDate {unLegSettlDate :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity LegSettlDate

instance IsField LegSettlDate where
  fieldTag Proxy = 588
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegSettlDate
  fieldFromValue = fromValue >=> (prettyValidate . LegSettlDate)
