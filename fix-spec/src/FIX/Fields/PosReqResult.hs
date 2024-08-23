{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.PosReqResult where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 728, fieldName = "PosReqResult", fieldType = FieldTypeInt, fieldValues = [FieldValueSpec {fieldValueEnum = "0", fieldValueDescription = "VALID_REQUEST"},FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "INVALID_OR_UNSUPPORTED_REQUEST"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "NO_POSITIONS_FOUND_THAT_MATCH_CRITERIA"},FieldValueSpec {fieldValueEnum = "3", fieldValueDescription = "NOT_AUTHORIZED_TO_REQUEST_POSITIONS"},FieldValueSpec {fieldValueEnum = "4", fieldValueDescription = "REQUEST_FOR_POSITION_NOT_SUPPORTED"},FieldValueSpec {fieldValueEnum = "99", fieldValueDescription = "OTHER"}]}
data PosReqResult
  = PosReqResult_VALID_REQUEST
  | PosReqResult_INVALID_OR_UNSUPPORTED_REQUEST
  | PosReqResult_NO_POSITIONS_FOUND_THAT_MATCH_CRITERIA
  | PosReqResult_NOT_AUTHORIZED_TO_REQUEST_POSITIONS
  | PosReqResult_REQUEST_FOR_POSITION_NOT_SUPPORTED
  | PosReqResult_OTHER
  deriving stock (Show, Eq, Generic)

instance Validity PosReqResult

instance IsField PosReqResult where
  fieldTag Proxy = 728
  fieldIsData Proxy = False
  fieldToValue = \case
    PosReqResult_VALID_REQUEST -> "0"
    PosReqResult_INVALID_OR_UNSUPPORTED_REQUEST -> "1"
    PosReqResult_NO_POSITIONS_FOUND_THAT_MATCH_CRITERIA -> "2"
    PosReqResult_NOT_AUTHORIZED_TO_REQUEST_POSITIONS -> "3"
    PosReqResult_REQUEST_FOR_POSITION_NOT_SUPPORTED -> "4"
    PosReqResult_OTHER -> "99"
  fieldFromValue = \case
    "0" -> Right PosReqResult_VALID_REQUEST
    "1" -> Right PosReqResult_INVALID_OR_UNSUPPORTED_REQUEST
    "2" -> Right PosReqResult_NO_POSITIONS_FOUND_THAT_MATCH_CRITERIA
    "3" -> Right PosReqResult_NOT_AUTHORIZED_TO_REQUEST_POSITIONS
    "4" -> Right PosReqResult_REQUEST_FOR_POSITION_NOT_SUPPORTED
    "99" -> Right PosReqResult_OTHER
    v -> Left ("Unknown PosReqResult: " <> show v)
