{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.MinBidSize where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 647
--   , fieldName = "MinBidSize"
--   , fieldType = FieldTypeQTY
--   , fieldValues = []
--   }
newtype MinBidSize = MinBidSize {unMinBidSize :: Qty}
  deriving stock (Show, Eq, Generic)

instance Validity MinBidSize

instance IsField MinBidSize where
  fieldTag Proxy = 647
  fieldIsData Proxy = False
  fieldToValue = toValue . unMinBidSize
  fieldFromValue = fromValue >=> (prettyValidate . MinBidSize)
