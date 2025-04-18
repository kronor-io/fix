{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.EncodedText where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 355
--   , fieldName = "EncodedText"
--   , fieldType = FieldTypeData
--   , fieldValues = []
--   }
newtype EncodedText = EncodedText {unEncodedText :: DataBytes}
  deriving stock (Show, Eq, Generic)

instance Validity EncodedText

instance IsField EncodedText where
  fieldTag Proxy = 355
  fieldIsData Proxy = True
  fieldToValue = toValue . unEncodedText
  fieldFromValue = fromValue >=> (prettyValidate . EncodedText)
