{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.RegistRejReasonText where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 496, fieldName = "RegistRejReasonText", fieldType = FieldTypeString, fieldValues = []}
newtype RegistRejReasonText = RegistRejReasonText {unRegistRejReasonText :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity RegistRejReasonText

instance IsField RegistRejReasonText where
  fieldTag Proxy = 496
  fieldIsData Proxy = False
  fieldToValue = toValue . unRegistRejReasonText
  fieldFromValue = fromValue >=> (prettyValidate . RegistRejReasonText)
