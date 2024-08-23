{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SecurityStatusReqID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 324, fieldName = "SecurityStatusReqID", fieldType = FieldTypeString, fieldValues = []}
newtype SecurityStatusReqID = SecurityStatusReqID {unSecurityStatusReqID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity SecurityStatusReqID

instance IsField SecurityStatusReqID where
  fieldTag Proxy = 324
  fieldIsData Proxy = False
  fieldToValue = toValue . unSecurityStatusReqID
  fieldFromValue = fromValue >=> (prettyValidate . SecurityStatusReqID)
