{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.QuoteType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 537
--   , fieldName = "QuoteType"
--   , fieldType = FieldTypeInt
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "0" , fieldValueDescription = "INDICATIVE" }
--       , FieldValueSpec
--           { fieldValueEnum = "1" , fieldValueDescription = "TRADEABLE" }
--       , FieldValueSpec
--           { fieldValueEnum = "2"
--           , fieldValueDescription = "RESTRICTED_TRADEABLE"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "3" , fieldValueDescription = "COUNTER" }
--       ]
--   }
data QuoteType
  = QuoteTypeIndicative
  | QuoteTypeTradeable
  | QuoteTypeRestrictedTradeable
  | QuoteTypeCounter
  deriving stock (Show, Eq, Generic)

instance Validity QuoteType

instance IsField QuoteType where
  fieldTag Proxy = 537
  fieldIsData Proxy = False
  fieldToValue = \case
    QuoteTypeIndicative -> "0"
    QuoteTypeTradeable -> "1"
    QuoteTypeRestrictedTradeable -> "2"
    QuoteTypeCounter -> "3"
  fieldFromValue = \case
    "0" -> Right QuoteTypeIndicative
    "1" -> Right QuoteTypeTradeable
    "2" -> Right QuoteTypeRestrictedTradeable
    "3" -> Right QuoteTypeCounter
    v -> Left ("Unknown QuoteType: " <> show v)