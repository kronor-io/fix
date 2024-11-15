{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.QuoteQualifier where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 695
--   , fieldName = "QuoteQualifier"
--   , fieldType = FieldTypeChar
--   , fieldValues = []
--   }
newtype QuoteQualifier = QuoteQualifier {unQuoteQualifier :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity QuoteQualifier

instance IsField QuoteQualifier where
  fieldTag Proxy = 695
  fieldIsData Proxy = False
  fieldToValue = toValue . unQuoteQualifier
  fieldFromValue = fromValue >=> (prettyValidate . QuoteQualifier)
