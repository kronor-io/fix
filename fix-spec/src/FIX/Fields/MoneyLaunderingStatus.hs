{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.MoneyLaunderingStatus where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 481
--   , fieldName = "MoneyLaunderingStatus"
--   , fieldType = FieldTypeChar
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "Y" , fieldValueDescription = "PASSED" }
--       , FieldValueSpec
--           { fieldValueEnum = "N" , fieldValueDescription = "NOT_CHECKED" }
--       , FieldValueSpec
--           { fieldValueEnum = "1"
--           , fieldValueDescription = "EXEMPT_BELOW_THE_LIMIT"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "2"
--           , fieldValueDescription = "EXEMPT_CLIENT_MONEY_TYPE_EXEMPTION"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "3"
--           , fieldValueDescription =
--               "EXEMPT_AUTHORISED_CREDIT_OR_FINANCIAL_INSTITUTION"
--           }
--       ]
--   }
data MoneyLaunderingStatus
  = MoneyLaunderingStatusPassed
  | MoneyLaunderingStatusNotChecked
  | MoneyLaunderingStatusExemptBelowTheLimit
  | MoneyLaunderingStatusExemptClientMoneyTypeExemption
  | MoneyLaunderingStatusExemptAuthorisedCreditOrFinancialInstitution
  deriving stock (Show, Eq, Generic)

instance Validity MoneyLaunderingStatus

instance IsField MoneyLaunderingStatus where
  fieldTag Proxy = 481
  fieldIsData Proxy = False
  fieldToValue = \case
    MoneyLaunderingStatusPassed -> "Y"
    MoneyLaunderingStatusNotChecked -> "N"
    MoneyLaunderingStatusExemptBelowTheLimit -> "1"
    MoneyLaunderingStatusExemptClientMoneyTypeExemption -> "2"
    MoneyLaunderingStatusExemptAuthorisedCreditOrFinancialInstitution -> "3"
  fieldFromValue = \case
    "Y" -> Right MoneyLaunderingStatusPassed
    "N" -> Right MoneyLaunderingStatusNotChecked
    "1" -> Right MoneyLaunderingStatusExemptBelowTheLimit
    "2" -> Right MoneyLaunderingStatusExemptClientMoneyTypeExemption
    "3" -> Right MoneyLaunderingStatusExemptAuthorisedCreditOrFinancialInstitution
    v -> Left ("Unknown MoneyLaunderingStatus: " <> show v)
