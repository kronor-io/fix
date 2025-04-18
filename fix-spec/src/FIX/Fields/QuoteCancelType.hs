{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.QuoteCancelType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 298
--   , fieldName = "QuoteCancelType"
--   , fieldType = FieldTypeInt
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "1"
--           , fieldValueDescription = "CANCEL_FOR_SYMBOL"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "2"
--           , fieldValueDescription = "CANCEL_FOR_SECURITY_TYPE"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "3"
--           , fieldValueDescription = "CANCEL_FOR_UNDERLYING_SYMBOL"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "4"
--           , fieldValueDescription = "CANCEL_ALL_QUOTES"
--           }
--       ]
--   }
data QuoteCancelType
  = QuoteCancelTypeCancelForSymbol
  | QuoteCancelTypeCancelForSecurityType
  | QuoteCancelTypeCancelForUnderlyingSymbol
  | QuoteCancelTypeCancelAllQuotes
  deriving stock (Show, Eq, Generic)

instance Validity QuoteCancelType

instance IsField QuoteCancelType where
  fieldTag Proxy = 298
  fieldIsData Proxy = False
  fieldToValue = \case
    QuoteCancelTypeCancelForSymbol -> "1"
    QuoteCancelTypeCancelForSecurityType -> "2"
    QuoteCancelTypeCancelForUnderlyingSymbol -> "3"
    QuoteCancelTypeCancelAllQuotes -> "4"
  fieldFromValue = \case
    "1" -> Right QuoteCancelTypeCancelForSymbol
    "2" -> Right QuoteCancelTypeCancelForSecurityType
    "3" -> Right QuoteCancelTypeCancelForUnderlyingSymbol
    "4" -> Right QuoteCancelTypeCancelAllQuotes
    v -> Left ("Unknown QuoteCancelType: " <> show v)
