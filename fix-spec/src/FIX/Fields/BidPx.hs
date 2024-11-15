{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.BidPx where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 132
--   , fieldName = "BidPx"
--   , fieldType = FieldTypePrice
--   , fieldValues = []
--   }
newtype BidPx = BidPx {unBidPx :: PriceVal}
  deriving stock (Show, Eq, Generic)

instance Validity BidPx

instance IsField BidPx where
  fieldTag Proxy = 132
  fieldIsData Proxy = False
  fieldToValue = toValue . unBidPx
  fieldFromValue = fromValue >=> (prettyValidate . BidPx)
