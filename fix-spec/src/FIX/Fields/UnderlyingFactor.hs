{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.UnderlyingFactor where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 246
--   , fieldName = "UnderlyingFactor"
--   , fieldType = FieldTypeFloat
--   , fieldValues = []
--   }
newtype UnderlyingFactor = UnderlyingFactor {unUnderlyingFactor :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity UnderlyingFactor

instance IsField UnderlyingFactor where
  fieldTag Proxy = 246
  fieldIsData Proxy = False
  fieldToValue = toValue . unUnderlyingFactor
  fieldFromValue = fromValue >=> (prettyValidate . UnderlyingFactor)
