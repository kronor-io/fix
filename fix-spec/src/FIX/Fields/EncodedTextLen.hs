{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.EncodedTextLen where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec {fieldNumber = 354, fieldName = "EncodedTextLen", fieldType = FieldTypeLength, fieldValues = []}
newtype EncodedTextLen = EncodedTextLen {unEncodedTextLen :: Word}
  deriving stock (Show, Eq, Generic)

instance Validity EncodedTextLen

instance IsField EncodedTextLen where
  fieldTag Proxy = 354
  fieldIsData Proxy = False
  fieldToValue = toValue . unEncodedTextLen
  fieldFromValue = fromValue >=> (prettyValidate . EncodedTextLen)
