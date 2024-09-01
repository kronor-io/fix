{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegProduct where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 607
--   , fieldName = "LegProduct"
--   , fieldType = FieldTypeInt
--   , fieldValues = []
--   }
newtype LegProduct = LegProduct {unLegProduct :: Int}
  deriving stock (Show, Eq, Generic)

instance Validity LegProduct

instance IsField LegProduct where
  fieldTag Proxy = 607
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegProduct
  fieldFromValue = fromValue >=> (prettyValidate . LegProduct)