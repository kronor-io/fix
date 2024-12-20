{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.ContraTrader where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 337
--   , fieldName = "ContraTrader"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype ContraTrader = ContraTrader {unContraTrader :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity ContraTrader

instance IsField ContraTrader where
  fieldTag Proxy = 337
  fieldIsData Proxy = False
  fieldToValue = toValue . unContraTrader
  fieldFromValue = fromValue >=> (prettyValidate . ContraTrader)
