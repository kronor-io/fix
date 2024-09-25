{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.UnderlyingSecuritySubType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 763
--   , fieldName = "UnderlyingSecuritySubType"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype UnderlyingSecuritySubType = UnderlyingSecuritySubType {unUnderlyingSecuritySubType :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity UnderlyingSecuritySubType

instance IsField UnderlyingSecuritySubType where
  fieldTag Proxy = 763
  fieldIsData Proxy = False
  fieldToValue = toValue . unUnderlyingSecuritySubType
  fieldFromValue = fromValue >=> (prettyValidate . UnderlyingSecuritySubType)