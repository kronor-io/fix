{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegOfferExAnteCost where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 9805
--   , fieldName = "LegOfferExAnteCost"
--   , fieldType = FieldTypePrice
--   , fieldValues = []
--   }
newtype LegOfferExAnteCost = LegOfferExAnteCost {unLegOfferExAnteCost :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity LegOfferExAnteCost

instance IsField LegOfferExAnteCost where
  fieldTag Proxy = 9805
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegOfferExAnteCost
  fieldFromValue = fromValue >=> (prettyValidate . LegOfferExAnteCost)
