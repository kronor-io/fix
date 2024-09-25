{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.FarLegRatePrecision where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 14006
--   , fieldName = "FarLegRatePrecision"
--   , fieldType = FieldTypeInt
--   , fieldValues = []
--   }
newtype FarLegRatePrecision = FarLegRatePrecision {unFarLegRatePrecision :: Int}
  deriving stock (Show, Eq, Generic)

instance Validity FarLegRatePrecision

instance IsField FarLegRatePrecision where
  fieldTag Proxy = 14006
  fieldIsData Proxy = False
  fieldToValue = toValue . unFarLegRatePrecision
  fieldFromValue = fromValue >=> (prettyValidate . FarLegRatePrecision)
