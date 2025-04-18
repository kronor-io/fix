{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.EncodedSecurityDesc where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 351
--   , fieldName = "EncodedSecurityDesc"
--   , fieldType = FieldTypeData
--   , fieldValues = []
--   }
newtype EncodedSecurityDesc = EncodedSecurityDesc {unEncodedSecurityDesc :: DataBytes}
  deriving stock (Show, Eq, Generic)

instance Validity EncodedSecurityDesc

instance IsField EncodedSecurityDesc where
  fieldTag Proxy = 351
  fieldIsData Proxy = True
  fieldToValue = toValue . unEncodedSecurityDesc
  fieldFromValue = fromValue >=> (prettyValidate . EncodedSecurityDesc)
