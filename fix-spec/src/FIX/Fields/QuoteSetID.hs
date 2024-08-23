{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.QuoteSetID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 302, fieldName = "QuoteSetID", fieldType = FieldTypeString, fieldValues = []}
newtype QuoteSetID = QuoteSetID {unQuoteSetID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity QuoteSetID

instance IsField QuoteSetID where
  fieldTag Proxy = 302
  fieldIsData Proxy = False
  fieldToValue = toValue . unQuoteSetID
  fieldFromValue = fromValue >=> (prettyValidate . QuoteSetID)
