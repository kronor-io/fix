{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SecurityRequestType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 321
--   , fieldName = "SecurityRequestType"
--   , fieldType = FieldTypeInt
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "0"
--           , fieldValueDescription =
--               "REQUEST_SECURITY_IDENTITY_AND_SPECIFICATIONS"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "1"
--           , fieldValueDescription =
--               "REQUEST_SECURITY_IDENTITY_FOR_THE_SPECIFICATIONS_PROVIDED"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "2"
--           , fieldValueDescription = "REQUEST_LIST_SECURITY_TYPES"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "3"
--           , fieldValueDescription = "REQUEST_LIST_SECURITIES"
--           }
--       ]
--   }
data SecurityRequestType
  = SecurityRequestTypeRequestSecurityIdentityAndSpecifications
  | SecurityRequestTypeRequestSecurityIdentityForTheSpecificationsProvided
  | SecurityRequestTypeRequestListSecurityTypes
  | SecurityRequestTypeRequestListSecurities
  deriving stock (Show, Eq, Generic)

instance Validity SecurityRequestType

instance IsField SecurityRequestType where
  fieldTag Proxy = 321
  fieldIsData Proxy = False
  fieldToValue = \case
    SecurityRequestTypeRequestSecurityIdentityAndSpecifications -> "0"
    SecurityRequestTypeRequestSecurityIdentityForTheSpecificationsProvided -> "1"
    SecurityRequestTypeRequestListSecurityTypes -> "2"
    SecurityRequestTypeRequestListSecurities -> "3"
  fieldFromValue = \case
    "0" -> Right SecurityRequestTypeRequestSecurityIdentityAndSpecifications
    "1" -> Right SecurityRequestTypeRequestSecurityIdentityForTheSpecificationsProvided
    "2" -> Right SecurityRequestTypeRequestListSecurityTypes
    "3" -> Right SecurityRequestTypeRequestListSecurities
    v -> Left ("Unknown SecurityRequestType: " <> show v)
