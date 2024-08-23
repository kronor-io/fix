{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.InvestorCountryOfResidence where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 475, fieldName = "InvestorCountryOfResidence", fieldType = FieldTypeCountry, fieldValues = []}
newtype InvestorCountryOfResidence = InvestorCountryOfResidence {unInvestorCountryOfResidence :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity InvestorCountryOfResidence

instance IsField InvestorCountryOfResidence where
  fieldTag Proxy = 475
  fieldIsData Proxy = False
  fieldToValue = toValue . unInvestorCountryOfResidence
  fieldFromValue = fromValue >=> (prettyValidate . InvestorCountryOfResidence)
