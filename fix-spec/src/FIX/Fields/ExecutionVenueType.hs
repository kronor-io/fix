{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.ExecutionVenueType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 7611
--   , fieldName = "ExecutionVenueType"
--   , fieldType = FieldTypeString
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "1" , fieldValueDescription = "SEF" }
--       , FieldValueSpec
--           { fieldValueEnum = "2" , fieldValueDescription = "OFF_FACILITY" }
--       , FieldValueSpec
--           { fieldValueEnum = "3" , fieldValueDescription = "MTF" }
--       , FieldValueSpec
--           { fieldValueEnum = "4" , fieldValueDescription = "SI" }
--       , FieldValueSpec
--           { fieldValueEnum = "5" , fieldValueDescription = "EU_MTF" }
--       , FieldValueSpec
--           { fieldValueEnum = "6" , fieldValueDescription = "UK_MTF" }
--       , FieldValueSpec
--           { fieldValueEnum = "7" , fieldValueDescription = "SG_RMO" }
--       ]
--   }
data ExecutionVenueType
  = ExecutionVenueTypeSef
  | ExecutionVenueTypeOffFacility
  | ExecutionVenueTypeMtf
  | ExecutionVenueTypeSi
  | ExecutionVenueTypeEuMtf
  | ExecutionVenueTypeUkMtf
  | ExecutionVenueTypeSgRmo
  deriving stock (Show, Eq, Generic)

instance Validity ExecutionVenueType

instance IsField ExecutionVenueType where
  fieldTag Proxy = 7611
  fieldIsData Proxy = False
  fieldToValue = \case
    ExecutionVenueTypeSef -> "1"
    ExecutionVenueTypeOffFacility -> "2"
    ExecutionVenueTypeMtf -> "3"
    ExecutionVenueTypeSi -> "4"
    ExecutionVenueTypeEuMtf -> "5"
    ExecutionVenueTypeUkMtf -> "6"
    ExecutionVenueTypeSgRmo -> "7"
  fieldFromValue = \case
    "1" -> Right ExecutionVenueTypeSef
    "2" -> Right ExecutionVenueTypeOffFacility
    "3" -> Right ExecutionVenueTypeMtf
    "4" -> Right ExecutionVenueTypeSi
    "5" -> Right ExecutionVenueTypeEuMtf
    "6" -> Right ExecutionVenueTypeUkMtf
    "7" -> Right ExecutionVenueTypeSgRmo
    v -> Left ("Unknown ExecutionVenueType: " <> show v)
