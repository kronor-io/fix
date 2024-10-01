{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.FixingDate where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 7543
--   , fieldName = "FixingDate"
--   , fieldType = FieldTypeLocalMktDate
--   , fieldValues = []
--   }
newtype FixingDate = FixingDate {unFixingDate :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity FixingDate

instance IsField FixingDate where
  fieldTag Proxy = 7543
  fieldIsData Proxy = False
  fieldToValue = toValue . unFixingDate
  fieldFromValue = fromValue >=> (prettyValidate . FixingDate)
