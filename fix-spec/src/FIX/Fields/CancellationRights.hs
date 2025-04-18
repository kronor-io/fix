{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.CancellationRights where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 480
--   , fieldName = "CancellationRights"
--   , fieldType = FieldTypeChar
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "N"
--           , fieldValueDescription = "NO_EXECUTION_ONLY"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "M"
--           , fieldValueDescription = "NO_WAIVER_AGREEMENT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "O"
--           , fieldValueDescription = "NO_INSTITUTIONAL"
--           }
--       ]
--   }
data CancellationRights
  = CancellationRightsNoExecutionOnly
  | CancellationRightsNoWaiverAgreement
  | CancellationRightsNoInstitutional
  deriving stock (Show, Eq, Generic)

instance Validity CancellationRights

instance IsField CancellationRights where
  fieldTag Proxy = 480
  fieldIsData Proxy = False
  fieldToValue = \case
    CancellationRightsNoExecutionOnly -> "N"
    CancellationRightsNoWaiverAgreement -> "M"
    CancellationRightsNoInstitutional -> "O"
  fieldFromValue = \case
    "N" -> Right CancellationRightsNoExecutionOnly
    "M" -> Right CancellationRightsNoWaiverAgreement
    "O" -> Right CancellationRightsNoInstitutional
    v -> Left ("Unknown CancellationRights: " <> show v)
