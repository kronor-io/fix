{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.Urgency where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 61
--   , fieldName = "Urgency"
--   , fieldType = FieldTypeChar
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "0" , fieldValueDescription = "NORMAL" }
--       , FieldValueSpec
--           { fieldValueEnum = "1" , fieldValueDescription = "FLASH" }
--       , FieldValueSpec
--           { fieldValueEnum = "2" , fieldValueDescription = "BACKGROUND" }
--       ]
--   }
data Urgency
  = UrgencyNormal
  | UrgencyFlash
  | UrgencyBackground
  deriving stock (Show, Eq, Generic)

instance Validity Urgency

instance IsField Urgency where
  fieldTag Proxy = 61
  fieldIsData Proxy = False
  fieldToValue = \case
    UrgencyNormal -> "0"
    UrgencyFlash -> "1"
    UrgencyBackground -> "2"
  fieldFromValue = \case
    "0" -> Right UrgencyNormal
    "1" -> Right UrgencyFlash
    "2" -> Right UrgencyBackground
    v -> Left ("Unknown Urgency: " <> show v)