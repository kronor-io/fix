{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.ContractSettlMonth where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 667
--   , fieldName = "ContractSettlMonth"
--   , fieldType = FieldTypeMonthYear
--   , fieldValues = []
--   }
newtype ContractSettlMonth = ContractSettlMonth {unContractSettlMonth :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity ContractSettlMonth

instance IsField ContractSettlMonth where
  fieldTag Proxy = 667
  fieldIsData Proxy = False
  fieldToValue = toValue . unContractSettlMonth
  fieldFromValue = fromValue >=> (prettyValidate . ContractSettlMonth)
