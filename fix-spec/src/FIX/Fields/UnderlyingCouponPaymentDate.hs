{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.UnderlyingCouponPaymentDate where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 241, fieldName = "UnderlyingCouponPaymentDate", fieldType = FieldTypeLocalMktDate, fieldValues = []}
newtype UnderlyingCouponPaymentDate = UnderlyingCouponPaymentDate {unUnderlyingCouponPaymentDate :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity UnderlyingCouponPaymentDate

instance IsField UnderlyingCouponPaymentDate where
  fieldTag Proxy = 241
  fieldIsData Proxy = False
  fieldToValue = toValue . unUnderlyingCouponPaymentDate
  fieldFromValue = fromValue >=> (prettyValidate . UnderlyingCouponPaymentDate)
