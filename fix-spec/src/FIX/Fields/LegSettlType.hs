{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegSettlType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 587
--   , fieldName = "LegSettlType"
--   , fieldType = FieldTypeChar
--   , fieldValues = []
--   }
newtype LegSettlType = LegSettlType {unLegSettlType :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity LegSettlType

instance IsField LegSettlType where
  fieldTag Proxy = 587
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegSettlType
  fieldFromValue = fromValue >=> (prettyValidate . LegSettlType)
