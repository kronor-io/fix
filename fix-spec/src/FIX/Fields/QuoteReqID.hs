{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.QuoteReqID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 131
--   , fieldName = "QuoteReqID"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype QuoteReqID = QuoteReqID {unQuoteReqID :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity QuoteReqID

instance IsField QuoteReqID where
  fieldTag Proxy = 131
  fieldIsData Proxy = False
  fieldToValue = toValue . unQuoteReqID
  fieldFromValue = fromValue >=> (prettyValidate . QuoteReqID)