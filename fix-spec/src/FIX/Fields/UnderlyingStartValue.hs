{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.UnderlyingStartValue where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 884
--   , fieldName = "UnderlyingStartValue"
--   , fieldType = FieldTypeAMT
--   , fieldValues = []
--   }
newtype UnderlyingStartValue = UnderlyingStartValue {unUnderlyingStartValue :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity UnderlyingStartValue

instance IsField UnderlyingStartValue where
  fieldTag Proxy = 884
  fieldIsData Proxy = False
  fieldToValue = toValue . unUnderlyingStartValue
  fieldFromValue = fromValue >=> (prettyValidate . UnderlyingStartValue)
