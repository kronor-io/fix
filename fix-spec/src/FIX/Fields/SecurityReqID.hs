{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SecurityReqID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 320
--   , fieldName = "SecurityReqID"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype SecurityReqID = SecurityReqID {unSecurityReqID :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity SecurityReqID

instance IsField SecurityReqID where
  fieldTag Proxy = 320
  fieldIsData Proxy = False
  fieldToValue = toValue . unSecurityReqID
  fieldFromValue = fromValue >=> (prettyValidate . SecurityReqID)
