{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.QuoteRequestType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 303
--   , fieldName = "QuoteRequestType"
--   , fieldType = FieldTypeInt
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "1" , fieldValueDescription = "MANUAL" }
--       , FieldValueSpec
--           { fieldValueEnum = "2" , fieldValueDescription = "AUTOMATIC" }
--       ]
--   }
data QuoteRequestType
  = QuoteRequestTypeManual
  | QuoteRequestTypeAutomatic
  deriving stock (Show, Eq, Generic)

instance Validity QuoteRequestType

instance IsField QuoteRequestType where
  fieldTag Proxy = 303
  fieldIsData Proxy = False
  fieldToValue = \case
    QuoteRequestTypeManual -> "1"
    QuoteRequestTypeAutomatic -> "2"
  fieldFromValue = \case
    "1" -> Right QuoteRequestTypeManual
    "2" -> Right QuoteRequestTypeAutomatic
    v -> Left ("Unknown QuoteRequestType: " <> show v)
