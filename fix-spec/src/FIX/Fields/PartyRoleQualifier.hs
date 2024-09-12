{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.PartyRoleQualifier where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 2376
--   , fieldName = "PartyRoleQualifier"
--   , fieldType = FieldTypeInt
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "22" , fieldValueDescription = "Algorithm" }
--       , FieldValueSpec
--           { fieldValueEnum = "24"
--           , fieldValueDescription = "Natural Person"
--           }
--       ]
--   }
data PartyRoleQualifier
  = PartyRoleQualifierAlgorithm
  | PartyRoleQualifierNaturalPerson
  deriving stock (Show, Eq, Generic)

instance Validity PartyRoleQualifier

instance IsField PartyRoleQualifier where
  fieldTag Proxy = 2376
  fieldIsData Proxy = False
  fieldToValue = \case
    PartyRoleQualifierAlgorithm -> "22"
    PartyRoleQualifierNaturalPerson -> "24"
  fieldFromValue = \case
    "22" -> Right PartyRoleQualifierAlgorithm
    "24" -> Right PartyRoleQualifierNaturalPerson
    v -> Left ("Unknown PartyRoleQualifier: " <> show v)
