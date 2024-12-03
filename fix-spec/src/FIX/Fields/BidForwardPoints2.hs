{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.BidForwardPoints2 where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 642
--   , fieldName = "BidForwardPoints2"
--   , fieldType = FieldTypePriceOffset
--   , fieldValues = []
--   }
newtype BidForwardPoints2 = BidForwardPoints2 {unBidForwardPoints2 :: PriceOffset}
  deriving stock (Show, Eq, Generic)

instance Validity BidForwardPoints2

instance IsField BidForwardPoints2 where
  fieldTag Proxy = 642
  fieldIsData Proxy = False
  fieldToValue = toValue . unBidForwardPoints2
  fieldFromValue = fromValue >=> (prettyValidate . BidForwardPoints2)
