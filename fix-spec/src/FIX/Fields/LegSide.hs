{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegSide where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 624
--   , fieldName = "LegSide"
--   , fieldType = FieldTypeChar
--   , fieldValues = []
--   }
newtype LegSide = LegSide {unLegSide :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity LegSide

instance IsField LegSide where
  fieldTag Proxy = 624
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegSide
  fieldFromValue = fromValue >=> (prettyValidate . LegSide)
