{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SecurityAltIDSource where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 456
--   , fieldName = "SecurityAltIDSource"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype SecurityAltIDSource = SecurityAltIDSource {unSecurityAltIDSource :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity SecurityAltIDSource

instance IsField SecurityAltIDSource where
  fieldTag Proxy = 456
  fieldIsData Proxy = False
  fieldToValue = toValue . unSecurityAltIDSource
  fieldFromValue = fromValue >=> (prettyValidate . SecurityAltIDSource)
