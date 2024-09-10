{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.OptAttribute where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 206
--   , fieldName = "OptAttribute"
--   , fieldType = FieldTypeChar
--   , fieldValues = []
--   }
newtype OptAttribute = OptAttribute {unOptAttribute :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity OptAttribute

instance IsField OptAttribute where
  fieldTag Proxy = 206
  fieldIsData Proxy = False
  fieldToValue = toValue . unOptAttribute
  fieldFromValue = fromValue >=> (prettyValidate . OptAttribute)
