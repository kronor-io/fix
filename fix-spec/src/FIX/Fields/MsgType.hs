{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.MsgType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 35
--   , fieldName = "MsgType"
--   , fieldType = FieldTypeString
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "0" , fieldValueDescription = "HEARTBEAT" }
--       , FieldValueSpec
--           { fieldValueEnum = "1" , fieldValueDescription = "TEST_REQUEST" }
--       , FieldValueSpec
--           { fieldValueEnum = "2" , fieldValueDescription = "RESEND_REQUEST" }
--       , FieldValueSpec
--           { fieldValueEnum = "3" , fieldValueDescription = "REJECT" }
--       , FieldValueSpec
--           { fieldValueEnum = "4" , fieldValueDescription = "SEQUENCE_RESET" }
--       , FieldValueSpec
--           { fieldValueEnum = "5" , fieldValueDescription = "LOGOUT" }
--       , FieldValueSpec
--           { fieldValueEnum = "6"
--           , fieldValueDescription = "INDICATION_OF_INTEREST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "7" , fieldValueDescription = "ADVERTISEMENT" }
--       , FieldValueSpec
--           { fieldValueEnum = "8"
--           , fieldValueDescription = "EXECUTION_REPORT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "9"
--           , fieldValueDescription = "ORDER_CANCEL_REJECT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "A" , fieldValueDescription = "LOGON" }
--       , FieldValueSpec
--           { fieldValueEnum = "B" , fieldValueDescription = "NEWS" }
--       , FieldValueSpec
--           { fieldValueEnum = "C" , fieldValueDescription = "EMAIL" }
--       , FieldValueSpec
--           { fieldValueEnum = "D"
--           , fieldValueDescription = "NEW_ORDER_SINGLE"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "E" , fieldValueDescription = "ORDER_LIST" }
--       , FieldValueSpec
--           { fieldValueEnum = "F"
--           , fieldValueDescription = "ORDER_CANCEL_REQUEST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "G"
--           , fieldValueDescription = "ORDER_CANCEL_REPLACE_REQUEST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "H"
--           , fieldValueDescription = "ORDER_STATUS_REQUEST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "J"
--           , fieldValueDescription = "ALLOCATION_INSTRUCTION"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "K"
--           , fieldValueDescription = "LIST_CANCEL_REQUEST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "L" , fieldValueDescription = "LIST_EXECUTE" }
--       , FieldValueSpec
--           { fieldValueEnum = "M"
--           , fieldValueDescription = "LIST_STATUS_REQUEST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "N" , fieldValueDescription = "LIST_STATUS" }
--       , FieldValueSpec
--           { fieldValueEnum = "P"
--           , fieldValueDescription = "ALLOCATION_INSTRUCTION_ACK"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "Q"
--           , fieldValueDescription = "DONT_KNOW_TRADE"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "R" , fieldValueDescription = "QUOTE_REQUEST" }
--       , FieldValueSpec
--           { fieldValueEnum = "S" , fieldValueDescription = "QUOTE" }
--       , FieldValueSpec
--           { fieldValueEnum = "T"
--           , fieldValueDescription = "SETTLEMENT_INSTRUCTIONS"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "V"
--           , fieldValueDescription = "MARKET_DATA_REQUEST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "W"
--           , fieldValueDescription = "MARKET_DATA_SNAPSHOT_FULL_REFRESH"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "X"
--           , fieldValueDescription = "MARKET_DATA_INCREMENTAL_REFRESH"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "Y"
--           , fieldValueDescription = "MARKET_DATA_REQUEST_REJECT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "Z" , fieldValueDescription = "QUOTE_CANCEL" }
--       , FieldValueSpec
--           { fieldValueEnum = "a"
--           , fieldValueDescription = "QUOTE_STATUS_REQUEST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "b"
--           , fieldValueDescription = "MASS_QUOTE_ACKNOWLEDGEMENT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "c"
--           , fieldValueDescription = "SECURITY_DEFINITION_REQUEST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "d"
--           , fieldValueDescription = "SECURITY_DEFINITION"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "e"
--           , fieldValueDescription = "SECURITY_STATUS_REQUEST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "f"
--           , fieldValueDescription = "SECURITY_STATUS"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "g"
--           , fieldValueDescription = "TRADING_SESSION_STATUS_REQUEST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "h"
--           , fieldValueDescription = "TRADING_SESSION_STATUS"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "i" , fieldValueDescription = "MASS_QUOTE" }
--       , FieldValueSpec
--           { fieldValueEnum = "j"
--           , fieldValueDescription = "BUSINESS_MESSAGE_REJECT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "k" , fieldValueDescription = "BID_REQUEST" }
--       , FieldValueSpec
--           { fieldValueEnum = "l" , fieldValueDescription = "BID_RESPONSE" }
--       , FieldValueSpec
--           { fieldValueEnum = "m"
--           , fieldValueDescription = "LIST_STRIKE_PRICE"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "n" , fieldValueDescription = "XML_MESSAGE" }
--       , FieldValueSpec
--           { fieldValueEnum = "o"
--           , fieldValueDescription = "REGISTRATION_INSTRUCTIONS"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "p"
--           , fieldValueDescription = "REGISTRATION_INSTRUCTIONS_RESPONSE"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "q"
--           , fieldValueDescription = "ORDER_MASS_CANCEL_REQUEST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "r"
--           , fieldValueDescription = "ORDER_MASS_CANCEL_REPORT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "s"
--           , fieldValueDescription = "NEW_ORDER_CROSS"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "t"
--           , fieldValueDescription = "CROSS_ORDER_CANCEL_REPLACE_REQUEST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "u"
--           , fieldValueDescription = "CROSS_ORDER_CANCEL_REQUEST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "v"
--           , fieldValueDescription = "SECURITY_TYPE_REQUEST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "w" , fieldValueDescription = "SECURITY_TYPES" }
--       , FieldValueSpec
--           { fieldValueEnum = "x"
--           , fieldValueDescription = "SECURITY_LIST_REQUEST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "y" , fieldValueDescription = "SECURITY_LIST" }
--       , FieldValueSpec
--           { fieldValueEnum = "z"
--           , fieldValueDescription = "DERIVATIVE_SECURITY_LIST_REQUEST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AA"
--           , fieldValueDescription = "DERIVATIVE_SECURITY_LIST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AB"
--           , fieldValueDescription = "NEW_ORDER_MULTILEG"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AC"
--           , fieldValueDescription = "MULTILEG_ORDER_CANCEL_REPLACE"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AD"
--           , fieldValueDescription = "TRADE_CAPTURE_REPORT_REQUEST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AE"
--           , fieldValueDescription = "TRADE_CAPTURE_REPORT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AF"
--           , fieldValueDescription = "ORDER_MASS_STATUS_REQUEST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AG"
--           , fieldValueDescription = "QUOTE_REQUEST_REJECT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AH" , fieldValueDescription = "RFQ_REQUEST" }
--       , FieldValueSpec
--           { fieldValueEnum = "AI"
--           , fieldValueDescription = "QUOTE_STATUS_REPORT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AJ"
--           , fieldValueDescription = "QUOTE_RESPONSE"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AK" , fieldValueDescription = "CONFIRMATION" }
--       , FieldValueSpec
--           { fieldValueEnum = "AL"
--           , fieldValueDescription = "POSITION_MAINTENANCE_REQUEST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AM"
--           , fieldValueDescription = "POSITION_MAINTENANCE_REPORT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AN"
--           , fieldValueDescription = "REQUEST_FOR_POSITIONS"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AO"
--           , fieldValueDescription = "REQUEST_FOR_POSITIONS_ACK"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AP"
--           , fieldValueDescription = "POSITION_REPORT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AQ"
--           , fieldValueDescription = "TRADE_CAPTURE_REPORT_REQUEST_ACK"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AR"
--           , fieldValueDescription = "TRADE_CAPTURE_REPORT_ACK"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AS"
--           , fieldValueDescription = "ALLOCATION_REPORT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AT"
--           , fieldValueDescription = "ALLOCATION_REPORT_ACK"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AU"
--           , fieldValueDescription = "CONFIRMATION_ACK"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AV"
--           , fieldValueDescription = "SETTLEMENT_INSTRUCTION_REQUEST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AW"
--           , fieldValueDescription = "ASSIGNMENT_REPORT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AX"
--           , fieldValueDescription = "COLLATERAL_REQUEST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AY"
--           , fieldValueDescription = "COLLATERAL_ASSIGNMENT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "AZ"
--           , fieldValueDescription = "COLLATERAL_RESPONSE"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "BA"
--           , fieldValueDescription = "COLLATERAL_REPORT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "BB"
--           , fieldValueDescription = "COLLATERAL_INQUIRY"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "BC"
--           , fieldValueDescription = "NETWORK_STATUS_REQUEST"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "BD"
--           , fieldValueDescription = "NETWORK_STATUS_RESPONSE"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "BE" , fieldValueDescription = "USER_REQUEST" }
--       , FieldValueSpec
--           { fieldValueEnum = "BF" , fieldValueDescription = "USER_RESPONSE" }
--       , FieldValueSpec
--           { fieldValueEnum = "BG"
--           , fieldValueDescription = "COLLATERAL_INQUIRY_ACK"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "BH"
--           , fieldValueDescription = "CONFIRMATION_REQUEST"
--           }
--       ]
--   }
data MsgType
  = MsgTypeHeartbeat
  | MsgTypeTestRequest
  | MsgTypeResendRequest
  | MsgTypeReject
  | MsgTypeSequenceReset
  | MsgTypeLogout
  | MsgTypeIndicationOfInterest
  | MsgTypeAdvertisement
  | MsgTypeExecutionReport
  | MsgTypeOrderCancelReject
  | MsgTypeLogon
  | MsgTypeNews
  | MsgTypeEmail
  | MsgTypeNewOrderSingle
  | MsgTypeOrderList
  | MsgTypeOrderCancelRequest
  | MsgTypeOrderCancelReplaceRequest
  | MsgTypeOrderStatusRequest
  | MsgTypeAllocationInstruction
  | MsgTypeListCancelRequest
  | MsgTypeListExecute
  | MsgTypeListStatusRequest
  | MsgTypeListStatus
  | MsgTypeAllocationInstructionAck
  | MsgTypeDontKnowTrade
  | MsgTypeQuoteRequest
  | MsgTypeQuote
  | MsgTypeSettlementInstructions
  | MsgTypeMarketDataRequest
  | MsgTypeMarketDataSnapshotFullRefresh
  | MsgTypeMarketDataIncrementalRefresh
  | MsgTypeMarketDataRequestReject
  | MsgTypeQuoteCancel
  | MsgTypeQuoteStatusRequest
  | MsgTypeMassQuoteAcknowledgement
  | MsgTypeSecurityDefinitionRequest
  | MsgTypeSecurityDefinition
  | MsgTypeSecurityStatusRequest
  | MsgTypeSecurityStatus
  | MsgTypeTradingSessionStatusRequest
  | MsgTypeTradingSessionStatus
  | MsgTypeMassQuote
  | MsgTypeBusinessMessageReject
  | MsgTypeBidRequest
  | MsgTypeBidResponse
  | MsgTypeListStrikePrice
  | MsgTypeXmlMessage
  | MsgTypeRegistrationInstructions
  | MsgTypeRegistrationInstructionsResponse
  | MsgTypeOrderMassCancelRequest
  | MsgTypeOrderMassCancelReport
  | MsgTypeNewOrderCross
  | MsgTypeCrossOrderCancelReplaceRequest
  | MsgTypeCrossOrderCancelRequest
  | MsgTypeSecurityTypeRequest
  | MsgTypeSecurityTypes
  | MsgTypeSecurityListRequest
  | MsgTypeSecurityList
  | MsgTypeDerivativeSecurityListRequest
  | MsgTypeDerivativeSecurityList
  | MsgTypeNewOrderMultileg
  | MsgTypeMultilegOrderCancelReplace
  | MsgTypeTradeCaptureReportRequest
  | MsgTypeTradeCaptureReport
  | MsgTypeOrderMassStatusRequest
  | MsgTypeQuoteRequestReject
  | MsgTypeRfqRequest
  | MsgTypeQuoteStatusReport
  | MsgTypeQuoteResponse
  | MsgTypeConfirmation
  | MsgTypePositionMaintenanceRequest
  | MsgTypePositionMaintenanceReport
  | MsgTypeRequestForPositions
  | MsgTypeRequestForPositionsAck
  | MsgTypePositionReport
  | MsgTypeTradeCaptureReportRequestAck
  | MsgTypeTradeCaptureReportAck
  | MsgTypeAllocationReport
  | MsgTypeAllocationReportAck
  | MsgTypeConfirmationAck
  | MsgTypeSettlementInstructionRequest
  | MsgTypeAssignmentReport
  | MsgTypeCollateralRequest
  | MsgTypeCollateralAssignment
  | MsgTypeCollateralResponse
  | MsgTypeCollateralReport
  | MsgTypeCollateralInquiry
  | MsgTypeNetworkStatusRequest
  | MsgTypeNetworkStatusResponse
  | MsgTypeUserRequest
  | MsgTypeUserResponse
  | MsgTypeCollateralInquiryAck
  | MsgTypeConfirmationRequest
  deriving stock (Show, Eq, Generic)

instance Validity MsgType

instance IsField MsgType where
  fieldTag Proxy = 35
  fieldIsData Proxy = False
  fieldToValue = \case
    MsgTypeHeartbeat -> "0"
    MsgTypeTestRequest -> "1"
    MsgTypeResendRequest -> "2"
    MsgTypeReject -> "3"
    MsgTypeSequenceReset -> "4"
    MsgTypeLogout -> "5"
    MsgTypeIndicationOfInterest -> "6"
    MsgTypeAdvertisement -> "7"
    MsgTypeExecutionReport -> "8"
    MsgTypeOrderCancelReject -> "9"
    MsgTypeLogon -> "A"
    MsgTypeNews -> "B"
    MsgTypeEmail -> "C"
    MsgTypeNewOrderSingle -> "D"
    MsgTypeOrderList -> "E"
    MsgTypeOrderCancelRequest -> "F"
    MsgTypeOrderCancelReplaceRequest -> "G"
    MsgTypeOrderStatusRequest -> "H"
    MsgTypeAllocationInstruction -> "J"
    MsgTypeListCancelRequest -> "K"
    MsgTypeListExecute -> "L"
    MsgTypeListStatusRequest -> "M"
    MsgTypeListStatus -> "N"
    MsgTypeAllocationInstructionAck -> "P"
    MsgTypeDontKnowTrade -> "Q"
    MsgTypeQuoteRequest -> "R"
    MsgTypeQuote -> "S"
    MsgTypeSettlementInstructions -> "T"
    MsgTypeMarketDataRequest -> "V"
    MsgTypeMarketDataSnapshotFullRefresh -> "W"
    MsgTypeMarketDataIncrementalRefresh -> "X"
    MsgTypeMarketDataRequestReject -> "Y"
    MsgTypeQuoteCancel -> "Z"
    MsgTypeQuoteStatusRequest -> "a"
    MsgTypeMassQuoteAcknowledgement -> "b"
    MsgTypeSecurityDefinitionRequest -> "c"
    MsgTypeSecurityDefinition -> "d"
    MsgTypeSecurityStatusRequest -> "e"
    MsgTypeSecurityStatus -> "f"
    MsgTypeTradingSessionStatusRequest -> "g"
    MsgTypeTradingSessionStatus -> "h"
    MsgTypeMassQuote -> "i"
    MsgTypeBusinessMessageReject -> "j"
    MsgTypeBidRequest -> "k"
    MsgTypeBidResponse -> "l"
    MsgTypeListStrikePrice -> "m"
    MsgTypeXmlMessage -> "n"
    MsgTypeRegistrationInstructions -> "o"
    MsgTypeRegistrationInstructionsResponse -> "p"
    MsgTypeOrderMassCancelRequest -> "q"
    MsgTypeOrderMassCancelReport -> "r"
    MsgTypeNewOrderCross -> "s"
    MsgTypeCrossOrderCancelReplaceRequest -> "t"
    MsgTypeCrossOrderCancelRequest -> "u"
    MsgTypeSecurityTypeRequest -> "v"
    MsgTypeSecurityTypes -> "w"
    MsgTypeSecurityListRequest -> "x"
    MsgTypeSecurityList -> "y"
    MsgTypeDerivativeSecurityListRequest -> "z"
    MsgTypeDerivativeSecurityList -> "AA"
    MsgTypeNewOrderMultileg -> "AB"
    MsgTypeMultilegOrderCancelReplace -> "AC"
    MsgTypeTradeCaptureReportRequest -> "AD"
    MsgTypeTradeCaptureReport -> "AE"
    MsgTypeOrderMassStatusRequest -> "AF"
    MsgTypeQuoteRequestReject -> "AG"
    MsgTypeRfqRequest -> "AH"
    MsgTypeQuoteStatusReport -> "AI"
    MsgTypeQuoteResponse -> "AJ"
    MsgTypeConfirmation -> "AK"
    MsgTypePositionMaintenanceRequest -> "AL"
    MsgTypePositionMaintenanceReport -> "AM"
    MsgTypeRequestForPositions -> "AN"
    MsgTypeRequestForPositionsAck -> "AO"
    MsgTypePositionReport -> "AP"
    MsgTypeTradeCaptureReportRequestAck -> "AQ"
    MsgTypeTradeCaptureReportAck -> "AR"
    MsgTypeAllocationReport -> "AS"
    MsgTypeAllocationReportAck -> "AT"
    MsgTypeConfirmationAck -> "AU"
    MsgTypeSettlementInstructionRequest -> "AV"
    MsgTypeAssignmentReport -> "AW"
    MsgTypeCollateralRequest -> "AX"
    MsgTypeCollateralAssignment -> "AY"
    MsgTypeCollateralResponse -> "AZ"
    MsgTypeCollateralReport -> "BA"
    MsgTypeCollateralInquiry -> "BB"
    MsgTypeNetworkStatusRequest -> "BC"
    MsgTypeNetworkStatusResponse -> "BD"
    MsgTypeUserRequest -> "BE"
    MsgTypeUserResponse -> "BF"
    MsgTypeCollateralInquiryAck -> "BG"
    MsgTypeConfirmationRequest -> "BH"
  fieldFromValue = \case
    "0" -> Right MsgTypeHeartbeat
    "1" -> Right MsgTypeTestRequest
    "2" -> Right MsgTypeResendRequest
    "3" -> Right MsgTypeReject
    "4" -> Right MsgTypeSequenceReset
    "5" -> Right MsgTypeLogout
    "6" -> Right MsgTypeIndicationOfInterest
    "7" -> Right MsgTypeAdvertisement
    "8" -> Right MsgTypeExecutionReport
    "9" -> Right MsgTypeOrderCancelReject
    "A" -> Right MsgTypeLogon
    "B" -> Right MsgTypeNews
    "C" -> Right MsgTypeEmail
    "D" -> Right MsgTypeNewOrderSingle
    "E" -> Right MsgTypeOrderList
    "F" -> Right MsgTypeOrderCancelRequest
    "G" -> Right MsgTypeOrderCancelReplaceRequest
    "H" -> Right MsgTypeOrderStatusRequest
    "J" -> Right MsgTypeAllocationInstruction
    "K" -> Right MsgTypeListCancelRequest
    "L" -> Right MsgTypeListExecute
    "M" -> Right MsgTypeListStatusRequest
    "N" -> Right MsgTypeListStatus
    "P" -> Right MsgTypeAllocationInstructionAck
    "Q" -> Right MsgTypeDontKnowTrade
    "R" -> Right MsgTypeQuoteRequest
    "S" -> Right MsgTypeQuote
    "T" -> Right MsgTypeSettlementInstructions
    "V" -> Right MsgTypeMarketDataRequest
    "W" -> Right MsgTypeMarketDataSnapshotFullRefresh
    "X" -> Right MsgTypeMarketDataIncrementalRefresh
    "Y" -> Right MsgTypeMarketDataRequestReject
    "Z" -> Right MsgTypeQuoteCancel
    "a" -> Right MsgTypeQuoteStatusRequest
    "b" -> Right MsgTypeMassQuoteAcknowledgement
    "c" -> Right MsgTypeSecurityDefinitionRequest
    "d" -> Right MsgTypeSecurityDefinition
    "e" -> Right MsgTypeSecurityStatusRequest
    "f" -> Right MsgTypeSecurityStatus
    "g" -> Right MsgTypeTradingSessionStatusRequest
    "h" -> Right MsgTypeTradingSessionStatus
    "i" -> Right MsgTypeMassQuote
    "j" -> Right MsgTypeBusinessMessageReject
    "k" -> Right MsgTypeBidRequest
    "l" -> Right MsgTypeBidResponse
    "m" -> Right MsgTypeListStrikePrice
    "n" -> Right MsgTypeXmlMessage
    "o" -> Right MsgTypeRegistrationInstructions
    "p" -> Right MsgTypeRegistrationInstructionsResponse
    "q" -> Right MsgTypeOrderMassCancelRequest
    "r" -> Right MsgTypeOrderMassCancelReport
    "s" -> Right MsgTypeNewOrderCross
    "t" -> Right MsgTypeCrossOrderCancelReplaceRequest
    "u" -> Right MsgTypeCrossOrderCancelRequest
    "v" -> Right MsgTypeSecurityTypeRequest
    "w" -> Right MsgTypeSecurityTypes
    "x" -> Right MsgTypeSecurityListRequest
    "y" -> Right MsgTypeSecurityList
    "z" -> Right MsgTypeDerivativeSecurityListRequest
    "AA" -> Right MsgTypeDerivativeSecurityList
    "AB" -> Right MsgTypeNewOrderMultileg
    "AC" -> Right MsgTypeMultilegOrderCancelReplace
    "AD" -> Right MsgTypeTradeCaptureReportRequest
    "AE" -> Right MsgTypeTradeCaptureReport
    "AF" -> Right MsgTypeOrderMassStatusRequest
    "AG" -> Right MsgTypeQuoteRequestReject
    "AH" -> Right MsgTypeRfqRequest
    "AI" -> Right MsgTypeQuoteStatusReport
    "AJ" -> Right MsgTypeQuoteResponse
    "AK" -> Right MsgTypeConfirmation
    "AL" -> Right MsgTypePositionMaintenanceRequest
    "AM" -> Right MsgTypePositionMaintenanceReport
    "AN" -> Right MsgTypeRequestForPositions
    "AO" -> Right MsgTypeRequestForPositionsAck
    "AP" -> Right MsgTypePositionReport
    "AQ" -> Right MsgTypeTradeCaptureReportRequestAck
    "AR" -> Right MsgTypeTradeCaptureReportAck
    "AS" -> Right MsgTypeAllocationReport
    "AT" -> Right MsgTypeAllocationReportAck
    "AU" -> Right MsgTypeConfirmationAck
    "AV" -> Right MsgTypeSettlementInstructionRequest
    "AW" -> Right MsgTypeAssignmentReport
    "AX" -> Right MsgTypeCollateralRequest
    "AY" -> Right MsgTypeCollateralAssignment
    "AZ" -> Right MsgTypeCollateralResponse
    "BA" -> Right MsgTypeCollateralReport
    "BB" -> Right MsgTypeCollateralInquiry
    "BC" -> Right MsgTypeNetworkStatusRequest
    "BD" -> Right MsgTypeNetworkStatusResponse
    "BE" -> Right MsgTypeUserRequest
    "BF" -> Right MsgTypeUserResponse
    "BG" -> Right MsgTypeCollateralInquiryAck
    "BH" -> Right MsgTypeConfirmationRequest
    v -> Left ("Unknown MsgType: " <> show v)
