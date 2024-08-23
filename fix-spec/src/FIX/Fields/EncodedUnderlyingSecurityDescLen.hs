{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.EncodedUnderlyingSecurityDescLen where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 364, fieldName = "EncodedUnderlyingSecurityDescLen", fieldType = FieldTypeLength, fieldValues = []}
newtype EncodedUnderlyingSecurityDescLen = EncodedUnderlyingSecurityDescLen {unEncodedUnderlyingSecurityDescLen :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity EncodedUnderlyingSecurityDescLen

instance IsField EncodedUnderlyingSecurityDescLen where
  fieldTag Proxy = 364
  fieldIsData Proxy = False
  fieldToValue = toValue . unEncodedUnderlyingSecurityDescLen
  fieldFromValue = fromValue >=> (prettyValidate . EncodedUnderlyingSecurityDescLen)
