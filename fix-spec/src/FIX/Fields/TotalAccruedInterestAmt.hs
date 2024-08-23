{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.TotalAccruedInterestAmt where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 540, fieldName = "TotalAccruedInterestAmt", fieldType = FieldTypeAMT, fieldValues = []}
newtype TotalAccruedInterestAmt = TotalAccruedInterestAmt {unTotalAccruedInterestAmt :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity TotalAccruedInterestAmt

instance IsField TotalAccruedInterestAmt where
  fieldTag Proxy = 540
  fieldIsData Proxy = False
  fieldToValue = toValue . unTotalAccruedInterestAmt
  fieldFromValue = fromValue >=> (prettyValidate . TotalAccruedInterestAmt)
