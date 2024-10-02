{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.Price where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 44
--   , fieldName = "Price"
--   , fieldType = FieldTypePrice
--   , fieldValues = []
--   }
newtype Price = Price {unPrice :: PriceVal}
  deriving stock (Show, Eq, Generic)

instance Validity Price

instance IsField Price where
  fieldTag Proxy = 44
  fieldIsData Proxy = False
  fieldToValue = toValue . unPrice
  fieldFromValue = fromValue >=> (prettyValidate . Price)
