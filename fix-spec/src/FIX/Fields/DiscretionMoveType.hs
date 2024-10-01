{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.DiscretionMoveType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 841
--   , fieldName = "DiscretionMoveType"
--   , fieldType = FieldTypeInt
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "0" , fieldValueDescription = "FLOATING" }
--       , FieldValueSpec
--           { fieldValueEnum = "1" , fieldValueDescription = "FIXED" }
--       ]
--   }
data DiscretionMoveType
  = DiscretionMoveTypeFloating
  | DiscretionMoveTypeFixed
  deriving stock (Show, Eq, Generic)

instance Validity DiscretionMoveType

instance IsField DiscretionMoveType where
  fieldTag Proxy = 841
  fieldIsData Proxy = False
  fieldToValue = \case
    DiscretionMoveTypeFloating -> "0"
    DiscretionMoveTypeFixed -> "1"
  fieldFromValue = \case
    "0" -> Right DiscretionMoveTypeFloating
    "1" -> Right DiscretionMoveTypeFixed
    v -> Left ("Unknown DiscretionMoveType: " <> show v)