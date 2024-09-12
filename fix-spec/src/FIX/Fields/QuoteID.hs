{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.QuoteID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 117
--   , fieldName = "QuoteID"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype QuoteID = QuoteID {unQuoteID :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity QuoteID

instance IsField QuoteID where
  fieldTag Proxy = 117
  fieldIsData Proxy = False
  fieldToValue = toValue . unQuoteID
  fieldFromValue = fromValue >=> (prettyValidate . QuoteID)