{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.EncodedLegIssuer where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 619
--   , fieldName = "EncodedLegIssuer"
--   , fieldType = FieldTypeData
--   , fieldValues = []
--   }
newtype EncodedLegIssuer = EncodedLegIssuer {unEncodedLegIssuer :: DataBytes}
  deriving stock (Show, Eq, Generic)

instance Validity EncodedLegIssuer

instance IsField EncodedLegIssuer where
  fieldTag Proxy = 619
  fieldIsData Proxy = True
  fieldToValue = toValue . unEncodedLegIssuer
  fieldFromValue = fromValue >=> (prettyValidate . EncodedLegIssuer)
