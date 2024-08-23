{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.Currency where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 15, fieldName = "Currency", fieldType = FieldTypeCurrency, fieldValues = []}
newtype Currency = Currency {unCurrency :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity Currency

instance IsField Currency where
  fieldTag Proxy = 15
  fieldIsData Proxy = False
  fieldToValue = toValue . unCurrency
  fieldFromValue = fromValue >=> (prettyValidate . Currency)
