{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.BidSize where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 134
--   , fieldName = "BidSize"
--   , fieldType = FieldTypeQTY
--   , fieldValues = []
--   }
newtype BidSize = BidSize {unBidSize :: Qty}
  deriving stock (Show, Eq, Generic)

instance Validity BidSize

instance IsField BidSize where
  fieldTag Proxy = 134
  fieldIsData Proxy = False
  fieldToValue = toValue . unBidSize
  fieldFromValue = fromValue >=> (prettyValidate . BidSize)
