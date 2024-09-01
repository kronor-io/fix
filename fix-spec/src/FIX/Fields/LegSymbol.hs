{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegSymbol where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 600
--   , fieldName = "LegSymbol"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype LegSymbol = LegSymbol {unLegSymbol :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity LegSymbol

instance IsField LegSymbol where
  fieldTag Proxy = 600
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegSymbol
  fieldFromValue = fromValue >=> (prettyValidate . LegSymbol)