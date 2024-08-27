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
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec {fieldNumber = 35, fieldName = "MsgType", fieldType = FieldTypeString, fieldValues = [FieldValueSpec {fieldValueEnum = "0", fieldValueDescription = "HEARTBEAT"},FieldValueSpec {fieldValueEnum = "1", fieldValueDescription = "TEST_REQUEST"},FieldValueSpec {fieldValueEnum = "2", fieldValueDescription = "RESEND_REQUEST"},FieldValueSpec {fieldValueEnum = "3", fieldValueDescription = "REJECT"},FieldValueSpec {fieldValueEnum = "4", fieldValueDescription = "SEQUENCE_RESET"},FieldValueSpec {fieldValueEnum = "5", fieldValueDescription = "LOGOUT"},FieldValueSpec {fieldValueEnum = "6", fieldValueDescription = "IOI"},FieldValueSpec {fieldValueEnum = "7", fieldValueDescription = "ADVERTISEMENT"},FieldValueSpec {fieldValueEnum = "8", fieldValueDescription = "EXECUTION_REPORT"},FieldValueSpec {fieldValueEnum = "9", fieldValueDescription = "ORDER_CANCEL_REJECT"},FieldValueSpec {fieldValueEnum = "A", fieldValueDescription = "LOGON"},FieldValueSpec {fieldValueEnum = "B", fieldValueDescription = "NEWS"},FieldValueSpec {fieldValueEnum = "C", fieldValueDescription = "EMAIL"},FieldValueSpec {fieldValueEnum = "D", fieldValueDescription = "NEW_ORDER_SINGLE"},FieldValueSpec {fieldValueEnum = "E", fieldValueDescription = "NEW_ORDER_LIST"},FieldValueSpec {fieldValueEnum = "F", fieldValueDescription = "ORDER_CANCEL_REQUEST"},FieldValueSpec {fieldValueEnum = "G", fieldValueDescription = "ORDER_CANCEL_REPLACE_REQUEST"},FieldValueSpec {fieldValueEnum = "H", fieldValueDescription = "ORDER_STATUS_REQUEST"},FieldValueSpec {fieldValueEnum = "J", fieldValueDescription = "ALLOCATION_INSTRUCTION"},FieldValueSpec {fieldValueEnum = "K", fieldValueDescription = "LIST_CANCEL_REQUEST"},FieldValueSpec {fieldValueEnum = "L", fieldValueDescription = "LIST_EXECUTE"},FieldValueSpec {fieldValueEnum = "M", fieldValueDescription = "LIST_STATUS_REQUEST"},FieldValueSpec {fieldValueEnum = "N", fieldValueDescription = "LIST_STATUS"},FieldValueSpec {fieldValueEnum = "P", fieldValueDescription = "ALLOCATION_INSTRUCTION_ACK"},FieldValueSpec {fieldValueEnum = "Q", fieldValueDescription = "DONT_KNOW_TRADE"},FieldValueSpec {fieldValueEnum = "R", fieldValueDescription = "QUOTE_REQUEST"},FieldValueSpec {fieldValueEnum = "S", fieldValueDescription = "QUOTE"},FieldValueSpec {fieldValueEnum = "T", fieldValueDescription = "SETTLEMENT_INSTRUCTIONS"},FieldValueSpec {fieldValueEnum = "V", fieldValueDescription = "MARKET_DATA_REQUEST"},FieldValueSpec {fieldValueEnum = "W", fieldValueDescription = "MARKET_DATA_SNAPSHOT_FULL_REFRESH"},FieldValueSpec {fieldValueEnum = "X", fieldValueDescription = "MARKET_DATA_INCREMENTAL_REFRESH"},FieldValueSpec {fieldValueEnum = "Y", fieldValueDescription = "MARKET_DATA_REQUEST_REJECT"},FieldValueSpec {fieldValueEnum = "Z", fieldValueDescription = "QUOTE_CANCEL"},FieldValueSpec {fieldValueEnum = "a", fieldValueDescription = "QUOTE_STATUS_REQUEST"},FieldValueSpec {fieldValueEnum = "b", fieldValueDescription = "MASS_QUOTE_ACKNOWLEDGEMENT"},FieldValueSpec {fieldValueEnum = "c", fieldValueDescription = "SECURITY_DEFINITION_REQUEST"},FieldValueSpec {fieldValueEnum = "d", fieldValueDescription = "SECURITY_DEFINITION"},FieldValueSpec {fieldValueEnum = "e", fieldValueDescription = "SECURITY_STATUS_REQUEST"},FieldValueSpec {fieldValueEnum = "f", fieldValueDescription = "SECURITY_STATUS"},FieldValueSpec {fieldValueEnum = "g", fieldValueDescription = "TRADING_SESSION_STATUS_REQUEST"},FieldValueSpec {fieldValueEnum = "h", fieldValueDescription = "TRADING_SESSION_STATUS"},FieldValueSpec {fieldValueEnum = "i", fieldValueDescription = "MASS_QUOTE"},FieldValueSpec {fieldValueEnum = "j", fieldValueDescription = "BUSINESS_MESSAGE_REJECT"},FieldValueSpec {fieldValueEnum = "k", fieldValueDescription = "BID_REQUEST"},FieldValueSpec {fieldValueEnum = "l", fieldValueDescription = "BID_RESPONSE"},FieldValueSpec {fieldValueEnum = "m", fieldValueDescription = "LIST_STRIKE_PRICE"},FieldValueSpec {fieldValueEnum = "n", fieldValueDescription = "XML_NON_FIX"},FieldValueSpec {fieldValueEnum = "o", fieldValueDescription = "REGISTRATION_INSTRUCTIONS"},FieldValueSpec {fieldValueEnum = "p", fieldValueDescription = "REGISTRATION_INSTRUCTIONS_RESPONSE"},FieldValueSpec {fieldValueEnum = "q", fieldValueDescription = "ORDER_MASS_CANCEL_REQUEST"},FieldValueSpec {fieldValueEnum = "r", fieldValueDescription = "ORDER_MASS_CANCEL_REPORT"},FieldValueSpec {fieldValueEnum = "s", fieldValueDescription = "NEW_ORDER_CROSS"},FieldValueSpec {fieldValueEnum = "t", fieldValueDescription = "CROSS_ORDER_CANCEL_REPLACE_REQUEST"},FieldValueSpec {fieldValueEnum = "u", fieldValueDescription = "CROSS_ORDER_CANCEL_REQUEST"},FieldValueSpec {fieldValueEnum = "v", fieldValueDescription = "SECURITY_TYPE_REQUEST"},FieldValueSpec {fieldValueEnum = "w", fieldValueDescription = "SECURITY_TYPES"},FieldValueSpec {fieldValueEnum = "x", fieldValueDescription = "SECURITY_LIST_REQUEST"},FieldValueSpec {fieldValueEnum = "y", fieldValueDescription = "SECURITY_LIST"},FieldValueSpec {fieldValueEnum = "z", fieldValueDescription = "DERIVATIVE_SECURITY_LIST_REQUEST"},FieldValueSpec {fieldValueEnum = "AA", fieldValueDescription = "DERIVATIVE_SECURITY_LIST"},FieldValueSpec {fieldValueEnum = "AB", fieldValueDescription = "NEW_ORDER_MULTILEG"},FieldValueSpec {fieldValueEnum = "AC", fieldValueDescription = "MULTILEG_ORDER_CANCEL_REPLACE"},FieldValueSpec {fieldValueEnum = "AD", fieldValueDescription = "TRADE_CAPTURE_REPORT_REQUEST"},FieldValueSpec {fieldValueEnum = "AE", fieldValueDescription = "TRADE_CAPTURE_REPORT"},FieldValueSpec {fieldValueEnum = "AF", fieldValueDescription = "ORDER_MASS_STATUS_REQUEST"},FieldValueSpec {fieldValueEnum = "AG", fieldValueDescription = "QUOTE_REQUEST_REJECT"},FieldValueSpec {fieldValueEnum = "AH", fieldValueDescription = "RFQ_REQUEST"},FieldValueSpec {fieldValueEnum = "AI", fieldValueDescription = "QUOTE_STATUS_REPORT"},FieldValueSpec {fieldValueEnum = "AJ", fieldValueDescription = "QUOTE_RESPONSE"},FieldValueSpec {fieldValueEnum = "AK", fieldValueDescription = "CONFIRMATION"},FieldValueSpec {fieldValueEnum = "AL", fieldValueDescription = "POSITION_MAINTENANCE_REQUEST"},FieldValueSpec {fieldValueEnum = "AM", fieldValueDescription = "POSITION_MAINTENANCE_REPORT"},FieldValueSpec {fieldValueEnum = "AN", fieldValueDescription = "REQUEST_FOR_POSITIONS"},FieldValueSpec {fieldValueEnum = "AO", fieldValueDescription = "REQUEST_FOR_POSITIONS_ACK"},FieldValueSpec {fieldValueEnum = "AP", fieldValueDescription = "POSITION_REPORT"},FieldValueSpec {fieldValueEnum = "AQ", fieldValueDescription = "TRADE_CAPTURE_REPORT_REQUEST_ACK"},FieldValueSpec {fieldValueEnum = "AR", fieldValueDescription = "TRADE_CAPTURE_REPORT_ACK"},FieldValueSpec {fieldValueEnum = "AS", fieldValueDescription = "ALLOCATION_REPORT"},FieldValueSpec {fieldValueEnum = "AT", fieldValueDescription = "ALLOCATION_REPORT_ACK"},FieldValueSpec {fieldValueEnum = "AU", fieldValueDescription = "CONFIRMATION_ACK"},FieldValueSpec {fieldValueEnum = "AV", fieldValueDescription = "SETTLEMENT_INSTRUCTION_REQUEST"},FieldValueSpec {fieldValueEnum = "AW", fieldValueDescription = "ASSIGNMENT_REPORT"},FieldValueSpec {fieldValueEnum = "AX", fieldValueDescription = "COLLATERAL_REQUEST"},FieldValueSpec {fieldValueEnum = "AY", fieldValueDescription = "COLLATERAL_ASSIGNMENT"},FieldValueSpec {fieldValueEnum = "AZ", fieldValueDescription = "COLLATERAL_RESPONSE"},FieldValueSpec {fieldValueEnum = "BA", fieldValueDescription = "COLLATERAL_REPORT"},FieldValueSpec {fieldValueEnum = "BB", fieldValueDescription = "COLLATERAL_INQUIRY"},FieldValueSpec {fieldValueEnum = "BC", fieldValueDescription = "NETWORK_COUNTERPARTY_SYSTEM_STATUS_REQUEST"},FieldValueSpec {fieldValueEnum = "BD", fieldValueDescription = "NETWORK_COUNTERPARTY_SYSTEM_STATUS_RESPONSE"},FieldValueSpec {fieldValueEnum = "BE", fieldValueDescription = "USER_REQUEST"},FieldValueSpec {fieldValueEnum = "BF", fieldValueDescription = "USER_RESPONSE"},FieldValueSpec {fieldValueEnum = "BG", fieldValueDescription = "COLLATERAL_INQUIRY_ACK"},FieldValueSpec {fieldValueEnum = "BH", fieldValueDescription = "CONFIRMATION_REQUEST"}]}
data MsgType
  = MsgType_HEARTBEAT
  | MsgType_TEST_REQUEST
  | MsgType_RESEND_REQUEST
  | MsgType_REJECT
  | MsgType_SEQUENCE_RESET
  | MsgType_LOGOUT
  | MsgType_IOI
  | MsgType_ADVERTISEMENT
  | MsgType_EXECUTION_REPORT
  | MsgType_ORDER_CANCEL_REJECT
  | MsgType_LOGON
  | MsgType_NEWS
  | MsgType_EMAIL
  | MsgType_NEW_ORDER_SINGLE
  | MsgType_NEW_ORDER_LIST
  | MsgType_ORDER_CANCEL_REQUEST
  | MsgType_ORDER_CANCEL_REPLACE_REQUEST
  | MsgType_ORDER_STATUS_REQUEST
  | MsgType_ALLOCATION_INSTRUCTION
  | MsgType_LIST_CANCEL_REQUEST
  | MsgType_LIST_EXECUTE
  | MsgType_LIST_STATUS_REQUEST
  | MsgType_LIST_STATUS
  | MsgType_ALLOCATION_INSTRUCTION_ACK
  | MsgType_DONT_KNOW_TRADE
  | MsgType_QUOTE_REQUEST
  | MsgType_QUOTE
  | MsgType_SETTLEMENT_INSTRUCTIONS
  | MsgType_MARKET_DATA_REQUEST
  | MsgType_MARKET_DATA_SNAPSHOT_FULL_REFRESH
  | MsgType_MARKET_DATA_INCREMENTAL_REFRESH
  | MsgType_MARKET_DATA_REQUEST_REJECT
  | MsgType_QUOTE_CANCEL
  | MsgType_QUOTE_STATUS_REQUEST
  | MsgType_MASS_QUOTE_ACKNOWLEDGEMENT
  | MsgType_SECURITY_DEFINITION_REQUEST
  | MsgType_SECURITY_DEFINITION
  | MsgType_SECURITY_STATUS_REQUEST
  | MsgType_SECURITY_STATUS
  | MsgType_TRADING_SESSION_STATUS_REQUEST
  | MsgType_TRADING_SESSION_STATUS
  | MsgType_MASS_QUOTE
  | MsgType_BUSINESS_MESSAGE_REJECT
  | MsgType_BID_REQUEST
  | MsgType_BID_RESPONSE
  | MsgType_LIST_STRIKE_PRICE
  | MsgType_XML_NON_FIX
  | MsgType_REGISTRATION_INSTRUCTIONS
  | MsgType_REGISTRATION_INSTRUCTIONS_RESPONSE
  | MsgType_ORDER_MASS_CANCEL_REQUEST
  | MsgType_ORDER_MASS_CANCEL_REPORT
  | MsgType_NEW_ORDER_CROSS
  | MsgType_CROSS_ORDER_CANCEL_REPLACE_REQUEST
  | MsgType_CROSS_ORDER_CANCEL_REQUEST
  | MsgType_SECURITY_TYPE_REQUEST
  | MsgType_SECURITY_TYPES
  | MsgType_SECURITY_LIST_REQUEST
  | MsgType_SECURITY_LIST
  | MsgType_DERIVATIVE_SECURITY_LIST_REQUEST
  | MsgType_DERIVATIVE_SECURITY_LIST
  | MsgType_NEW_ORDER_MULTILEG
  | MsgType_MULTILEG_ORDER_CANCEL_REPLACE
  | MsgType_TRADE_CAPTURE_REPORT_REQUEST
  | MsgType_TRADE_CAPTURE_REPORT
  | MsgType_ORDER_MASS_STATUS_REQUEST
  | MsgType_QUOTE_REQUEST_REJECT
  | MsgType_RFQ_REQUEST
  | MsgType_QUOTE_STATUS_REPORT
  | MsgType_QUOTE_RESPONSE
  | MsgType_CONFIRMATION
  | MsgType_POSITION_MAINTENANCE_REQUEST
  | MsgType_POSITION_MAINTENANCE_REPORT
  | MsgType_REQUEST_FOR_POSITIONS
  | MsgType_REQUEST_FOR_POSITIONS_ACK
  | MsgType_POSITION_REPORT
  | MsgType_TRADE_CAPTURE_REPORT_REQUEST_ACK
  | MsgType_TRADE_CAPTURE_REPORT_ACK
  | MsgType_ALLOCATION_REPORT
  | MsgType_ALLOCATION_REPORT_ACK
  | MsgType_CONFIRMATION_ACK
  | MsgType_SETTLEMENT_INSTRUCTION_REQUEST
  | MsgType_ASSIGNMENT_REPORT
  | MsgType_COLLATERAL_REQUEST
  | MsgType_COLLATERAL_ASSIGNMENT
  | MsgType_COLLATERAL_RESPONSE
  | MsgType_COLLATERAL_REPORT
  | MsgType_COLLATERAL_INQUIRY
  | MsgType_NETWORK_COUNTERPARTY_SYSTEM_STATUS_REQUEST
  | MsgType_NETWORK_COUNTERPARTY_SYSTEM_STATUS_RESPONSE
  | MsgType_USER_REQUEST
  | MsgType_USER_RESPONSE
  | MsgType_COLLATERAL_INQUIRY_ACK
  | MsgType_CONFIRMATION_REQUEST
  deriving stock (Show, Eq, Generic)

instance Validity MsgType

instance IsField MsgType where
  fieldTag Proxy = 35
  fieldIsData Proxy = False
  fieldToValue = \case
    MsgType_HEARTBEAT -> "0"
    MsgType_TEST_REQUEST -> "1"
    MsgType_RESEND_REQUEST -> "2"
    MsgType_REJECT -> "3"
    MsgType_SEQUENCE_RESET -> "4"
    MsgType_LOGOUT -> "5"
    MsgType_IOI -> "6"
    MsgType_ADVERTISEMENT -> "7"
    MsgType_EXECUTION_REPORT -> "8"
    MsgType_ORDER_CANCEL_REJECT -> "9"
    MsgType_LOGON -> "A"
    MsgType_NEWS -> "B"
    MsgType_EMAIL -> "C"
    MsgType_NEW_ORDER_SINGLE -> "D"
    MsgType_NEW_ORDER_LIST -> "E"
    MsgType_ORDER_CANCEL_REQUEST -> "F"
    MsgType_ORDER_CANCEL_REPLACE_REQUEST -> "G"
    MsgType_ORDER_STATUS_REQUEST -> "H"
    MsgType_ALLOCATION_INSTRUCTION -> "J"
    MsgType_LIST_CANCEL_REQUEST -> "K"
    MsgType_LIST_EXECUTE -> "L"
    MsgType_LIST_STATUS_REQUEST -> "M"
    MsgType_LIST_STATUS -> "N"
    MsgType_ALLOCATION_INSTRUCTION_ACK -> "P"
    MsgType_DONT_KNOW_TRADE -> "Q"
    MsgType_QUOTE_REQUEST -> "R"
    MsgType_QUOTE -> "S"
    MsgType_SETTLEMENT_INSTRUCTIONS -> "T"
    MsgType_MARKET_DATA_REQUEST -> "V"
    MsgType_MARKET_DATA_SNAPSHOT_FULL_REFRESH -> "W"
    MsgType_MARKET_DATA_INCREMENTAL_REFRESH -> "X"
    MsgType_MARKET_DATA_REQUEST_REJECT -> "Y"
    MsgType_QUOTE_CANCEL -> "Z"
    MsgType_QUOTE_STATUS_REQUEST -> "a"
    MsgType_MASS_QUOTE_ACKNOWLEDGEMENT -> "b"
    MsgType_SECURITY_DEFINITION_REQUEST -> "c"
    MsgType_SECURITY_DEFINITION -> "d"
    MsgType_SECURITY_STATUS_REQUEST -> "e"
    MsgType_SECURITY_STATUS -> "f"
    MsgType_TRADING_SESSION_STATUS_REQUEST -> "g"
    MsgType_TRADING_SESSION_STATUS -> "h"
    MsgType_MASS_QUOTE -> "i"
    MsgType_BUSINESS_MESSAGE_REJECT -> "j"
    MsgType_BID_REQUEST -> "k"
    MsgType_BID_RESPONSE -> "l"
    MsgType_LIST_STRIKE_PRICE -> "m"
    MsgType_XML_NON_FIX -> "n"
    MsgType_REGISTRATION_INSTRUCTIONS -> "o"
    MsgType_REGISTRATION_INSTRUCTIONS_RESPONSE -> "p"
    MsgType_ORDER_MASS_CANCEL_REQUEST -> "q"
    MsgType_ORDER_MASS_CANCEL_REPORT -> "r"
    MsgType_NEW_ORDER_CROSS -> "s"
    MsgType_CROSS_ORDER_CANCEL_REPLACE_REQUEST -> "t"
    MsgType_CROSS_ORDER_CANCEL_REQUEST -> "u"
    MsgType_SECURITY_TYPE_REQUEST -> "v"
    MsgType_SECURITY_TYPES -> "w"
    MsgType_SECURITY_LIST_REQUEST -> "x"
    MsgType_SECURITY_LIST -> "y"
    MsgType_DERIVATIVE_SECURITY_LIST_REQUEST -> "z"
    MsgType_DERIVATIVE_SECURITY_LIST -> "AA"
    MsgType_NEW_ORDER_MULTILEG -> "AB"
    MsgType_MULTILEG_ORDER_CANCEL_REPLACE -> "AC"
    MsgType_TRADE_CAPTURE_REPORT_REQUEST -> "AD"
    MsgType_TRADE_CAPTURE_REPORT -> "AE"
    MsgType_ORDER_MASS_STATUS_REQUEST -> "AF"
    MsgType_QUOTE_REQUEST_REJECT -> "AG"
    MsgType_RFQ_REQUEST -> "AH"
    MsgType_QUOTE_STATUS_REPORT -> "AI"
    MsgType_QUOTE_RESPONSE -> "AJ"
    MsgType_CONFIRMATION -> "AK"
    MsgType_POSITION_MAINTENANCE_REQUEST -> "AL"
    MsgType_POSITION_MAINTENANCE_REPORT -> "AM"
    MsgType_REQUEST_FOR_POSITIONS -> "AN"
    MsgType_REQUEST_FOR_POSITIONS_ACK -> "AO"
    MsgType_POSITION_REPORT -> "AP"
    MsgType_TRADE_CAPTURE_REPORT_REQUEST_ACK -> "AQ"
    MsgType_TRADE_CAPTURE_REPORT_ACK -> "AR"
    MsgType_ALLOCATION_REPORT -> "AS"
    MsgType_ALLOCATION_REPORT_ACK -> "AT"
    MsgType_CONFIRMATION_ACK -> "AU"
    MsgType_SETTLEMENT_INSTRUCTION_REQUEST -> "AV"
    MsgType_ASSIGNMENT_REPORT -> "AW"
    MsgType_COLLATERAL_REQUEST -> "AX"
    MsgType_COLLATERAL_ASSIGNMENT -> "AY"
    MsgType_COLLATERAL_RESPONSE -> "AZ"
    MsgType_COLLATERAL_REPORT -> "BA"
    MsgType_COLLATERAL_INQUIRY -> "BB"
    MsgType_NETWORK_COUNTERPARTY_SYSTEM_STATUS_REQUEST -> "BC"
    MsgType_NETWORK_COUNTERPARTY_SYSTEM_STATUS_RESPONSE -> "BD"
    MsgType_USER_REQUEST -> "BE"
    MsgType_USER_RESPONSE -> "BF"
    MsgType_COLLATERAL_INQUIRY_ACK -> "BG"
    MsgType_CONFIRMATION_REQUEST -> "BH"
  fieldFromValue = \case
    "0" -> Right MsgType_HEARTBEAT
    "1" -> Right MsgType_TEST_REQUEST
    "2" -> Right MsgType_RESEND_REQUEST
    "3" -> Right MsgType_REJECT
    "4" -> Right MsgType_SEQUENCE_RESET
    "5" -> Right MsgType_LOGOUT
    "6" -> Right MsgType_IOI
    "7" -> Right MsgType_ADVERTISEMENT
    "8" -> Right MsgType_EXECUTION_REPORT
    "9" -> Right MsgType_ORDER_CANCEL_REJECT
    "A" -> Right MsgType_LOGON
    "B" -> Right MsgType_NEWS
    "C" -> Right MsgType_EMAIL
    "D" -> Right MsgType_NEW_ORDER_SINGLE
    "E" -> Right MsgType_NEW_ORDER_LIST
    "F" -> Right MsgType_ORDER_CANCEL_REQUEST
    "G" -> Right MsgType_ORDER_CANCEL_REPLACE_REQUEST
    "H" -> Right MsgType_ORDER_STATUS_REQUEST
    "J" -> Right MsgType_ALLOCATION_INSTRUCTION
    "K" -> Right MsgType_LIST_CANCEL_REQUEST
    "L" -> Right MsgType_LIST_EXECUTE
    "M" -> Right MsgType_LIST_STATUS_REQUEST
    "N" -> Right MsgType_LIST_STATUS
    "P" -> Right MsgType_ALLOCATION_INSTRUCTION_ACK
    "Q" -> Right MsgType_DONT_KNOW_TRADE
    "R" -> Right MsgType_QUOTE_REQUEST
    "S" -> Right MsgType_QUOTE
    "T" -> Right MsgType_SETTLEMENT_INSTRUCTIONS
    "V" -> Right MsgType_MARKET_DATA_REQUEST
    "W" -> Right MsgType_MARKET_DATA_SNAPSHOT_FULL_REFRESH
    "X" -> Right MsgType_MARKET_DATA_INCREMENTAL_REFRESH
    "Y" -> Right MsgType_MARKET_DATA_REQUEST_REJECT
    "Z" -> Right MsgType_QUOTE_CANCEL
    "a" -> Right MsgType_QUOTE_STATUS_REQUEST
    "b" -> Right MsgType_MASS_QUOTE_ACKNOWLEDGEMENT
    "c" -> Right MsgType_SECURITY_DEFINITION_REQUEST
    "d" -> Right MsgType_SECURITY_DEFINITION
    "e" -> Right MsgType_SECURITY_STATUS_REQUEST
    "f" -> Right MsgType_SECURITY_STATUS
    "g" -> Right MsgType_TRADING_SESSION_STATUS_REQUEST
    "h" -> Right MsgType_TRADING_SESSION_STATUS
    "i" -> Right MsgType_MASS_QUOTE
    "j" -> Right MsgType_BUSINESS_MESSAGE_REJECT
    "k" -> Right MsgType_BID_REQUEST
    "l" -> Right MsgType_BID_RESPONSE
    "m" -> Right MsgType_LIST_STRIKE_PRICE
    "n" -> Right MsgType_XML_NON_FIX
    "o" -> Right MsgType_REGISTRATION_INSTRUCTIONS
    "p" -> Right MsgType_REGISTRATION_INSTRUCTIONS_RESPONSE
    "q" -> Right MsgType_ORDER_MASS_CANCEL_REQUEST
    "r" -> Right MsgType_ORDER_MASS_CANCEL_REPORT
    "s" -> Right MsgType_NEW_ORDER_CROSS
    "t" -> Right MsgType_CROSS_ORDER_CANCEL_REPLACE_REQUEST
    "u" -> Right MsgType_CROSS_ORDER_CANCEL_REQUEST
    "v" -> Right MsgType_SECURITY_TYPE_REQUEST
    "w" -> Right MsgType_SECURITY_TYPES
    "x" -> Right MsgType_SECURITY_LIST_REQUEST
    "y" -> Right MsgType_SECURITY_LIST
    "z" -> Right MsgType_DERIVATIVE_SECURITY_LIST_REQUEST
    "AA" -> Right MsgType_DERIVATIVE_SECURITY_LIST
    "AB" -> Right MsgType_NEW_ORDER_MULTILEG
    "AC" -> Right MsgType_MULTILEG_ORDER_CANCEL_REPLACE
    "AD" -> Right MsgType_TRADE_CAPTURE_REPORT_REQUEST
    "AE" -> Right MsgType_TRADE_CAPTURE_REPORT
    "AF" -> Right MsgType_ORDER_MASS_STATUS_REQUEST
    "AG" -> Right MsgType_QUOTE_REQUEST_REJECT
    "AH" -> Right MsgType_RFQ_REQUEST
    "AI" -> Right MsgType_QUOTE_STATUS_REPORT
    "AJ" -> Right MsgType_QUOTE_RESPONSE
    "AK" -> Right MsgType_CONFIRMATION
    "AL" -> Right MsgType_POSITION_MAINTENANCE_REQUEST
    "AM" -> Right MsgType_POSITION_MAINTENANCE_REPORT
    "AN" -> Right MsgType_REQUEST_FOR_POSITIONS
    "AO" -> Right MsgType_REQUEST_FOR_POSITIONS_ACK
    "AP" -> Right MsgType_POSITION_REPORT
    "AQ" -> Right MsgType_TRADE_CAPTURE_REPORT_REQUEST_ACK
    "AR" -> Right MsgType_TRADE_CAPTURE_REPORT_ACK
    "AS" -> Right MsgType_ALLOCATION_REPORT
    "AT" -> Right MsgType_ALLOCATION_REPORT_ACK
    "AU" -> Right MsgType_CONFIRMATION_ACK
    "AV" -> Right MsgType_SETTLEMENT_INSTRUCTION_REQUEST
    "AW" -> Right MsgType_ASSIGNMENT_REPORT
    "AX" -> Right MsgType_COLLATERAL_REQUEST
    "AY" -> Right MsgType_COLLATERAL_ASSIGNMENT
    "AZ" -> Right MsgType_COLLATERAL_RESPONSE
    "BA" -> Right MsgType_COLLATERAL_REPORT
    "BB" -> Right MsgType_COLLATERAL_INQUIRY
    "BC" -> Right MsgType_NETWORK_COUNTERPARTY_SYSTEM_STATUS_REQUEST
    "BD" -> Right MsgType_NETWORK_COUNTERPARTY_SYSTEM_STATUS_RESPONSE
    "BE" -> Right MsgType_USER_REQUEST
    "BF" -> Right MsgType_USER_RESPONSE
    "BG" -> Right MsgType_COLLATERAL_INQUIRY_ACK
    "BH" -> Right MsgType_CONFIRMATION_REQUEST
    v -> Left ("Unknown MsgType: " <> show v)