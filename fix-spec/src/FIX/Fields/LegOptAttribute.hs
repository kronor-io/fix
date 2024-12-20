{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegOptAttribute where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 613
--   , fieldName = "LegOptAttribute"
--   , fieldType = FieldTypeChar
--   , fieldValues = []
--   }
newtype LegOptAttribute = LegOptAttribute {unLegOptAttribute :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity LegOptAttribute

instance IsField LegOptAttribute where
  fieldTag Proxy = 613
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegOptAttribute
  fieldFromValue = fromValue >=> (prettyValidate . LegOptAttribute)
