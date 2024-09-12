{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.BidInterestAtMaturity where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 7084
--   , fieldName = "BidInterestAtMaturity"
--   , fieldType = FieldTypePrice
--   , fieldValues = []
--   }
newtype BidInterestAtMaturity = BidInterestAtMaturity {unBidInterestAtMaturity :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity BidInterestAtMaturity

instance IsField BidInterestAtMaturity where
  fieldTag Proxy = 7084
  fieldIsData Proxy = False
  fieldToValue = toValue . unBidInterestAtMaturity
  fieldFromValue = fromValue >=> (prettyValidate . BidInterestAtMaturity)