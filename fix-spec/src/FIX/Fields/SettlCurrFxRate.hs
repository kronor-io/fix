{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SettlCurrFxRate where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 155
--   , fieldName = "SettlCurrFxRate"
--   , fieldType = FieldTypeFloat
--   , fieldValues = []
--   }
newtype SettlCurrFxRate = SettlCurrFxRate {unSettlCurrFxRate :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity SettlCurrFxRate

instance IsField SettlCurrFxRate where
  fieldTag Proxy = 155
  fieldIsData Proxy = False
  fieldToValue = toValue . unSettlCurrFxRate
  fieldFromValue = fromValue >=> (prettyValidate . SettlCurrFxRate)
