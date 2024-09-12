{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.OfferExAnteCostPercentage where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 9803
--   , fieldName = "OfferExAnteCostPercentage"
--   , fieldType = FieldTypePercentage
--   , fieldValues = []
--   }
newtype OfferExAnteCostPercentage = OfferExAnteCostPercentage {unOfferExAnteCostPercentage :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity OfferExAnteCostPercentage

instance IsField OfferExAnteCostPercentage where
  fieldTag Proxy = 9803
  fieldIsData Proxy = False
  fieldToValue = toValue . unOfferExAnteCostPercentage
  fieldFromValue = fromValue >=> (prettyValidate . OfferExAnteCostPercentage)