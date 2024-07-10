{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegMaturityMonthYear where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 610
--   , fieldName = "LegMaturityMonthYear"
--   , fieldType = FieldTypeMonthYear
--   , fieldValues = []
--   }
newtype LegMaturityMonthYear = LegMaturityMonthYear {unLegMaturityMonthYear :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity LegMaturityMonthYear

instance IsField LegMaturityMonthYear where
  fieldTag Proxy = 610
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegMaturityMonthYear
  fieldFromValue = fromValue >=> (prettyValidate . LegMaturityMonthYear)
