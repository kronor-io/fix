{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.UnderlyingCFICode where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 463
--   , fieldName = "UnderlyingCFICode"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype UnderlyingCFICode = UnderlyingCFICode {unUnderlyingCFICode :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity UnderlyingCFICode

instance IsField UnderlyingCFICode where
  fieldTag Proxy = 463
  fieldIsData Proxy = False
  fieldToValue = toValue . unUnderlyingCFICode
  fieldFromValue = fromValue >=> (prettyValidate . UnderlyingCFICode)
