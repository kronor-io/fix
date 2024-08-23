{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.CollInquiryResult where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 946, fieldName = "CollInquiryResult", fieldType = FieldTypeInt, fieldValues = [FieldValueSpec {fieldValueEnum = "0", fieldValueDescription = "SUCCESSFUL"},FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "INVALID_OR_UNKNOWN_INSTRUMENT"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "INVALID_OR_UNKNOWN_COLLATERAL_TYPE"},FieldValueSpec {fieldValueEnum = "3", fieldValueDescription = "INVALID_PARTIES"},FieldValueSpec {fieldValueEnum = "4", fieldValueDescription = "INVALID_TRANSPORT_TYPE_REQUESTED"},FieldValueSpec {fieldValueEnum = "5", fieldValueDescription = "INVALID_DESTINATION_REQUESTED"},FieldValueSpec {fieldValueEnum = "6", fieldValueDescription = "NO_COLLATERAL_FOUND_FOR_THE_TRADE_SPECIFIED"},FieldValueSpec {fieldValueEnum = "7", fieldValueDescription = "NO_COLLATERAL_FOUND_FOR_THE_ORDER_SPECIFIED"},FieldValueSpec {fieldValueEnum = "8", fieldValueDescription = "COLLATERAL_INQUIRY_TYPE_NOT_SUPPORTED"},FieldValueSpec {fieldValueEnum = "9", fieldValueDescription = "UNAUTHORIZED_FOR_COLLATERAL_INQUIRY"},FieldValueSpec {fieldValueEnum = "99", fieldValueDescription = "OTHER"}]}
data CollInquiryResult
  = CollInquiryResult_SUCCESSFUL
  | CollInquiryResult_INVALID_OR_UNKNOWN_INSTRUMENT
  | CollInquiryResult_INVALID_OR_UNKNOWN_COLLATERAL_TYPE
  | CollInquiryResult_INVALID_PARTIES
  | CollInquiryResult_INVALID_TRANSPORT_TYPE_REQUESTED
  | CollInquiryResult_INVALID_DESTINATION_REQUESTED
  | CollInquiryResult_NO_COLLATERAL_FOUND_FOR_THE_TRADE_SPECIFIED
  | CollInquiryResult_NO_COLLATERAL_FOUND_FOR_THE_ORDER_SPECIFIED
  | CollInquiryResult_COLLATERAL_INQUIRY_TYPE_NOT_SUPPORTED
  | CollInquiryResult_UNAUTHORIZED_FOR_COLLATERAL_INQUIRY
  | CollInquiryResult_OTHER
  deriving stock (Show, Eq, Generic)

instance Validity CollInquiryResult

instance IsField CollInquiryResult where
  fieldTag Proxy = 946
  fieldIsData Proxy = False
  fieldToValue = \case
    CollInquiryResult_SUCCESSFUL -> "0"
    CollInquiryResult_INVALID_OR_UNKNOWN_INSTRUMENT -> "1"
    CollInquiryResult_INVALID_OR_UNKNOWN_COLLATERAL_TYPE -> "2"
    CollInquiryResult_INVALID_PARTIES -> "3"
    CollInquiryResult_INVALID_TRANSPORT_TYPE_REQUESTED -> "4"
    CollInquiryResult_INVALID_DESTINATION_REQUESTED -> "5"
    CollInquiryResult_NO_COLLATERAL_FOUND_FOR_THE_TRADE_SPECIFIED -> "6"
    CollInquiryResult_NO_COLLATERAL_FOUND_FOR_THE_ORDER_SPECIFIED -> "7"
    CollInquiryResult_COLLATERAL_INQUIRY_TYPE_NOT_SUPPORTED -> "8"
    CollInquiryResult_UNAUTHORIZED_FOR_COLLATERAL_INQUIRY -> "9"
    CollInquiryResult_OTHER -> "99"
  fieldFromValue = \case
    "0" -> Right CollInquiryResult_SUCCESSFUL
    "1" -> Right CollInquiryResult_INVALID_OR_UNKNOWN_INSTRUMENT
    "2" -> Right CollInquiryResult_INVALID_OR_UNKNOWN_COLLATERAL_TYPE
    "3" -> Right CollInquiryResult_INVALID_PARTIES
    "4" -> Right CollInquiryResult_INVALID_TRANSPORT_TYPE_REQUESTED
    "5" -> Right CollInquiryResult_INVALID_DESTINATION_REQUESTED
    "6" -> Right CollInquiryResult_NO_COLLATERAL_FOUND_FOR_THE_TRADE_SPECIFIED
    "7" -> Right CollInquiryResult_NO_COLLATERAL_FOUND_FOR_THE_ORDER_SPECIFIED
    "8" -> Right CollInquiryResult_COLLATERAL_INQUIRY_TYPE_NOT_SUPPORTED
    "9" -> Right CollInquiryResult_UNAUTHORIZED_FOR_COLLATERAL_INQUIRY
    "99" -> Right CollInquiryResult_OTHER
    v -> Left ("Unknown CollInquiryResult: " <> show v)
