{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.CoveredOrUncovered where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 203
--   , fieldName = "CoveredOrUncovered"
--   , fieldType = FieldTypeInt
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "0" , fieldValueDescription = "COVERED" }
--       , FieldValueSpec
--           { fieldValueEnum = "1" , fieldValueDescription = "UNCOVERED" }
--       ]
--   }
data CoveredOrUncovered
  = CoveredOrUncoveredCovered
  | CoveredOrUncoveredUncovered
  deriving stock (Show, Eq, Generic)

instance Validity CoveredOrUncovered

instance IsField CoveredOrUncovered where
  fieldTag Proxy = 203
  fieldIsData Proxy = False
  fieldToValue = \case
    CoveredOrUncoveredCovered -> "0"
    CoveredOrUncoveredUncovered -> "1"
  fieldFromValue = \case
    "0" -> Right CoveredOrUncoveredCovered
    "1" -> Right CoveredOrUncoveredUncovered
    v -> Left ("Unknown CoveredOrUncovered: " <> show v)
