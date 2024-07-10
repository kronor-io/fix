{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.UPICode where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 2891
--   , fieldName = "UPICode"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype UPICode = UPICode {unUPICode :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity UPICode

instance IsField UPICode where
  fieldTag Proxy = 2891
  fieldIsData Proxy = False
  fieldToValue = toValue . unUPICode
  fieldFromValue = fromValue >=> (prettyValidate . UPICode)
