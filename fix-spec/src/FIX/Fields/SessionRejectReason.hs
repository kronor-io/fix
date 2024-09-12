{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SessionRejectReason where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 373
--   , fieldName = "SessionRejectReason"
--   , fieldType = FieldTypeInt
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "0"
--           , fieldValueDescription = "INVALID_TAG_NUMBER"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "1"
--           , fieldValueDescription = "REQUIRED_TAG_MISSING"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "2"
--           , fieldValueDescription = "TAG_NOT_DEFINED_FOR_THIS_MESSAGE_TYPE"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "3" , fieldValueDescription = "UNDEFINED_TAG" }
--       , FieldValueSpec
--           { fieldValueEnum = "4"
--           , fieldValueDescription = "TAG_SPECIFIED_WITHOUT_A_VALUE"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "5"
--           , fieldValueDescription = "VALUE_IS_INCORRECT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "6"
--           , fieldValueDescription = "INCORRECT_DATA_FORMAT_FOR_VALUE"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "7"
--           , fieldValueDescription = "DECRYPTION_PROBLEM"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "8"
--           , fieldValueDescription = "SIGNATURE_PROBLEM"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "9"
--           , fieldValueDescription = "COMP_ID_PROBLEM"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "10"
--           , fieldValueDescription = "SENDING_TIME_ACCURACY_PROBLEM"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "11"
--           , fieldValueDescription = "INVALID_MSG_TYPE"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "12"
--           , fieldValueDescription = "XML_VALIDATION_ERROR"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "13"
--           , fieldValueDescription = "TAG_APPEARS_MORE_THAN_ONCE"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "14"
--           , fieldValueDescription = "TAG_SPECIFIED_OUT_OF_REQUIRED_ORDER"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "15"
--           , fieldValueDescription = "REPEATING_GROUP_FIELDS_OUT_OF_ORDER"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "16"
--           , fieldValueDescription =
--               "INCORRECT_NUM_IN_GROUP_COUNT_FOR_REPEATING_GROUP"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "17" , fieldValueDescription = "NON" }
--       , FieldValueSpec
--           { fieldValueEnum = "99" , fieldValueDescription = "OTHER" }
--       ]
--   }
data SessionRejectReason
  = SessionRejectReasonInvalidTagNumber
  | SessionRejectReasonRequiredTagMissing
  | SessionRejectReasonTagNotDefinedForThisMessageType
  | SessionRejectReasonUndefinedTag
  | SessionRejectReasonTagSpecifiedWithoutAValue
  | SessionRejectReasonValueIsIncorrect
  | SessionRejectReasonIncorrectDataFormatForValue
  | SessionRejectReasonDecryptionProblem
  | SessionRejectReasonSignatureProblem
  | SessionRejectReasonCompIdProblem
  | SessionRejectReasonSendingTimeAccuracyProblem
  | SessionRejectReasonInvalidMsgType
  | SessionRejectReasonXmlValidationError
  | SessionRejectReasonTagAppearsMoreThanOnce
  | SessionRejectReasonTagSpecifiedOutOfRequiredOrder
  | SessionRejectReasonRepeatingGroupFieldsOutOfOrder
  | SessionRejectReasonIncorrectNumInGroupCountForRepeatingGroup
  | SessionRejectReasonNon
  | SessionRejectReasonOther
  deriving stock (Show, Eq, Generic)

instance Validity SessionRejectReason

instance IsField SessionRejectReason where
  fieldTag Proxy = 373
  fieldIsData Proxy = False
  fieldToValue = \case
    SessionRejectReasonInvalidTagNumber -> "0"
    SessionRejectReasonRequiredTagMissing -> "1"
    SessionRejectReasonTagNotDefinedForThisMessageType -> "2"
    SessionRejectReasonUndefinedTag -> "3"
    SessionRejectReasonTagSpecifiedWithoutAValue -> "4"
    SessionRejectReasonValueIsIncorrect -> "5"
    SessionRejectReasonIncorrectDataFormatForValue -> "6"
    SessionRejectReasonDecryptionProblem -> "7"
    SessionRejectReasonSignatureProblem -> "8"
    SessionRejectReasonCompIdProblem -> "9"
    SessionRejectReasonSendingTimeAccuracyProblem -> "10"
    SessionRejectReasonInvalidMsgType -> "11"
    SessionRejectReasonXmlValidationError -> "12"
    SessionRejectReasonTagAppearsMoreThanOnce -> "13"
    SessionRejectReasonTagSpecifiedOutOfRequiredOrder -> "14"
    SessionRejectReasonRepeatingGroupFieldsOutOfOrder -> "15"
    SessionRejectReasonIncorrectNumInGroupCountForRepeatingGroup -> "16"
    SessionRejectReasonNon -> "17"
    SessionRejectReasonOther -> "99"
  fieldFromValue = \case
    "0" -> Right SessionRejectReasonInvalidTagNumber
    "1" -> Right SessionRejectReasonRequiredTagMissing
    "2" -> Right SessionRejectReasonTagNotDefinedForThisMessageType
    "3" -> Right SessionRejectReasonUndefinedTag
    "4" -> Right SessionRejectReasonTagSpecifiedWithoutAValue
    "5" -> Right SessionRejectReasonValueIsIncorrect
    "6" -> Right SessionRejectReasonIncorrectDataFormatForValue
    "7" -> Right SessionRejectReasonDecryptionProblem
    "8" -> Right SessionRejectReasonSignatureProblem
    "9" -> Right SessionRejectReasonCompIdProblem
    "10" -> Right SessionRejectReasonSendingTimeAccuracyProblem
    "11" -> Right SessionRejectReasonInvalidMsgType
    "12" -> Right SessionRejectReasonXmlValidationError
    "13" -> Right SessionRejectReasonTagAppearsMoreThanOnce
    "14" -> Right SessionRejectReasonTagSpecifiedOutOfRequiredOrder
    "15" -> Right SessionRejectReasonRepeatingGroupFieldsOutOfOrder
    "16" -> Right SessionRejectReasonIncorrectNumInGroupCountForRepeatingGroup
    "17" -> Right SessionRejectReasonNon
    "99" -> Right SessionRejectReasonOther
    v -> Left ("Unknown SessionRejectReason: " <> show v)
