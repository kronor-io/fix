{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.Spread where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 218
--   , fieldName = "Spread"
--   , fieldType = FieldTypePriceOffset
--   , fieldValues = []
--   }
newtype Spread = Spread {unSpread :: PriceOffset}
  deriving stock (Show, Eq, Generic)

instance Validity Spread

instance IsField Spread where
  fieldTag Proxy = 218
  fieldIsData Proxy = False
  fieldToValue = toValue . unSpread
  fieldFromValue = fromValue >=> (prettyValidate . Spread)
