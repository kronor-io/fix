{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.MultiLegRptTypeReq where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 563
--   , fieldName = "MultiLegRptTypeReq"
--   , fieldType = FieldTypeInt
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "0"
--           , fieldValueDescription = "REPORT_BY_MULITLEG_SECURITY_ONLY"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "1"
--           , fieldValueDescription =
--               "REPORT_BY_MULTILEG_SECURITY_AND_BY_INSTRUMENT_LEGS_BELONGING_TO_THE_MULTILEG_SECURITY"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "2"
--           , fieldValueDescription =
--               "REPORT_BY_INSTRUMENT_LEGS_BELONGING_TO_THE_MULTILEG_SECURITY_ONLY"
--           }
--       ]
--   }
data MultiLegRptTypeReq
  = MultiLegRptTypeReqReportByMulitlegSecurityOnly
  | MultiLegRptTypeReqReportByMultilegSecurityAndByInstrumentLegsBelongingToTheMultilegSecurity
  | MultiLegRptTypeReqReportByInstrumentLegsBelongingToTheMultilegSecurityOnly
  deriving stock (Show, Eq, Generic)

instance Validity MultiLegRptTypeReq

instance IsField MultiLegRptTypeReq where
  fieldTag Proxy = 563
  fieldIsData Proxy = False
  fieldToValue = \case
    MultiLegRptTypeReqReportByMulitlegSecurityOnly -> "0"
    MultiLegRptTypeReqReportByMultilegSecurityAndByInstrumentLegsBelongingToTheMultilegSecurity -> "1"
    MultiLegRptTypeReqReportByInstrumentLegsBelongingToTheMultilegSecurityOnly -> "2"
  fieldFromValue = \case
    "0" -> Right MultiLegRptTypeReqReportByMulitlegSecurityOnly
    "1" -> Right MultiLegRptTypeReqReportByMultilegSecurityAndByInstrumentLegsBelongingToTheMultilegSecurity
    "2" -> Right MultiLegRptTypeReqReportByInstrumentLegsBelongingToTheMultilegSecurityOnly
    v -> Left ("Unknown MultiLegRptTypeReq: " <> show v)
