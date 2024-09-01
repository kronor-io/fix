{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.Symbol where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 55
--   , fieldName = "Symbol"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype Symbol = Symbol {unSymbol :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity Symbol

instance IsField Symbol where
  fieldTag Proxy = 55
  fieldIsData Proxy = False
  fieldToValue = toValue . unSymbol
  fieldFromValue = fromValue >=> (prettyValidate . Symbol)