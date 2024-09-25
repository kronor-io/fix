{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegAllocQty where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 673
--   , fieldName = "LegAllocQty"
--   , fieldType = FieldTypeQTY
--   , fieldValues = []
--   }
newtype LegAllocQty = LegAllocQty {unLegAllocQty :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity LegAllocQty

instance IsField LegAllocQty where
  fieldTag Proxy = 673
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegAllocQty
  fieldFromValue = fromValue >=> (prettyValidate . LegAllocQty)