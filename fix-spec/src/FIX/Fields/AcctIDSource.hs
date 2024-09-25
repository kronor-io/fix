{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.AcctIDSource where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 660
--   , fieldName = "AcctIDSource"
--   , fieldType = FieldTypeInt
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "1" , fieldValueDescription = "BIC" }
--       , FieldValueSpec
--           { fieldValueEnum = "2" , fieldValueDescription = "SID_CODE" }
--       , FieldValueSpec
--           { fieldValueEnum = "3" , fieldValueDescription = "TFM" }
--       , FieldValueSpec
--           { fieldValueEnum = "4" , fieldValueDescription = "OMGEO" }
--       , FieldValueSpec
--           { fieldValueEnum = "5" , fieldValueDescription = "DTCC_CODE" }
--       ]
--   }
data AcctIDSource
  = AcctIDSourceBic
  | AcctIDSourceSidCode
  | AcctIDSourceTfm
  | AcctIDSourceOmgeo
  | AcctIDSourceDtccCode
  deriving stock (Show, Eq, Generic)

instance Validity AcctIDSource

instance IsField AcctIDSource where
  fieldTag Proxy = 660
  fieldIsData Proxy = False
  fieldToValue = \case
    AcctIDSourceBic -> "1"
    AcctIDSourceSidCode -> "2"
    AcctIDSourceTfm -> "3"
    AcctIDSourceOmgeo -> "4"
    AcctIDSourceDtccCode -> "5"
  fieldFromValue = \case
    "1" -> Right AcctIDSourceBic
    "2" -> Right AcctIDSourceSidCode
    "3" -> Right AcctIDSourceTfm
    "4" -> Right AcctIDSourceOmgeo
    "5" -> Right AcctIDSourceDtccCode
    v -> Left ("Unknown AcctIDSource: " <> show v)
