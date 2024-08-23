{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.BidSpotRate where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 188, fieldName = "BidSpotRate", fieldType = FieldTypePrice, fieldValues = []}
newtype BidSpotRate = BidSpotRate {unBidSpotRate :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity BidSpotRate

instance IsField BidSpotRate where
  fieldTag Proxy = 188
  fieldIsData Proxy = False
  fieldToValue = toValue . unBidSpotRate
  fieldFromValue = fromValue >=> (prettyValidate . BidSpotRate)
