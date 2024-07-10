{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.DayAvgPx where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 426
--   , fieldName = "DayAvgPx"
--   , fieldType = FieldTypePrice
--   , fieldValues = []
--   }
newtype DayAvgPx = DayAvgPx {unDayAvgPx :: PriceVal}
  deriving stock (Show, Eq, Generic)

instance Validity DayAvgPx

instance IsField DayAvgPx where
  fieldTag Proxy = 426
  fieldIsData Proxy = False
  fieldToValue = toValue . unDayAvgPx
  fieldFromValue = fromValue >=> (prettyValidate . DayAvgPx)
