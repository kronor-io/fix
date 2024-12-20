{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.PegScope where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 840
--   , fieldName = "PegScope"
--   , fieldType = FieldTypeInt
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "1" , fieldValueDescription = "LOCAL" }
--       , FieldValueSpec
--           { fieldValueEnum = "2" , fieldValueDescription = "NATIONAL" }
--       , FieldValueSpec
--           { fieldValueEnum = "3" , fieldValueDescription = "GLOBAL" }
--       , FieldValueSpec
--           { fieldValueEnum = "4"
--           , fieldValueDescription = "NATIONAL_EXCLUDING_LOCAL"
--           }
--       ]
--   }
data PegScope
  = PegScopeLocal
  | PegScopeNational
  | PegScopeGlobal
  | PegScopeNationalExcludingLocal
  deriving stock (Show, Eq, Generic)

instance Validity PegScope

instance IsField PegScope where
  fieldTag Proxy = 840
  fieldIsData Proxy = False
  fieldToValue = \case
    PegScopeLocal -> "1"
    PegScopeNational -> "2"
    PegScopeGlobal -> "3"
    PegScopeNationalExcludingLocal -> "4"
  fieldFromValue = \case
    "1" -> Right PegScopeLocal
    "2" -> Right PegScopeNational
    "3" -> Right PegScopeGlobal
    "4" -> Right PegScopeNationalExcludingLocal
    v -> Left ("Unknown PegScope: " <> show v)
