{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.UnderlyingInstrRegistry where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 595
--   , fieldName = "UnderlyingInstrRegistry"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype UnderlyingInstrRegistry = UnderlyingInstrRegistry {unUnderlyingInstrRegistry :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity UnderlyingInstrRegistry

instance IsField UnderlyingInstrRegistry where
  fieldTag Proxy = 595
  fieldIsData Proxy = False
  fieldToValue = toValue . unUnderlyingInstrRegistry
  fieldFromValue = fromValue >=> (prettyValidate . UnderlyingInstrRegistry)
