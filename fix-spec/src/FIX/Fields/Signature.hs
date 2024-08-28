{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.Signature where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 89
--   , fieldName = "Signature"
--   , fieldType = FieldTypeData
--   , fieldValues = []
--   }
newtype Signature = Signature {unSignature :: DataBytes}
  deriving stock (Show, Eq, Generic)

instance Validity Signature

instance IsField Signature where
  fieldTag Proxy = 89
  fieldIsData Proxy = True
  fieldToValue = toValue . unSignature
  fieldFromValue = fromValue >=> (prettyValidate . Signature)
