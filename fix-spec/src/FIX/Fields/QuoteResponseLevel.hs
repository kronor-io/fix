{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.QuoteResponseLevel where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 301
--   , fieldName = "QuoteResponseLevel"
--   , fieldType = FieldTypeInt
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "0"
--           , fieldValueDescription = "NO_ACKNOWLEDGEMENT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "1"
--           , fieldValueDescription =
--               "ACKNOWLEDGE_ONLY_NEGATIVE_OR_ERRONEOUS_QUOTES"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "2"
--           , fieldValueDescription = "ACKNOWLEDGE_EACH_QUOTE_MESSAGE"
--           }
--       ]
--   }
data QuoteResponseLevel
  = QuoteResponseLevelNoAcknowledgement
  | QuoteResponseLevelAcknowledgeOnlyNegativeOrErroneousQuotes
  | QuoteResponseLevelAcknowledgeEachQuoteMessage
  deriving stock (Show, Eq, Generic)

instance Validity QuoteResponseLevel

instance IsField QuoteResponseLevel where
  fieldTag Proxy = 301
  fieldIsData Proxy = False
  fieldToValue = \case
    QuoteResponseLevelNoAcknowledgement -> "0"
    QuoteResponseLevelAcknowledgeOnlyNegativeOrErroneousQuotes -> "1"
    QuoteResponseLevelAcknowledgeEachQuoteMessage -> "2"
  fieldFromValue = \case
    "0" -> Right QuoteResponseLevelNoAcknowledgement
    "1" -> Right QuoteResponseLevelAcknowledgeOnlyNegativeOrErroneousQuotes
    "2" -> Right QuoteResponseLevelAcknowledgeEachQuoteMessage
    v -> Left ("Unknown QuoteResponseLevel: " <> show v)
