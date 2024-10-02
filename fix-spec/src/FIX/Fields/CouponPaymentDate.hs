{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.CouponPaymentDate where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 224
--   , fieldName = "CouponPaymentDate"
--   , fieldType = FieldTypeLocalMktDate
--   , fieldValues = []
--   }
newtype CouponPaymentDate = CouponPaymentDate {unCouponPaymentDate :: LocalMktDate}
  deriving stock (Show, Eq, Generic)

instance Validity CouponPaymentDate

instance IsField CouponPaymentDate where
  fieldTag Proxy = 224
  fieldIsData Proxy = False
  fieldToValue = toValue . unCouponPaymentDate
  fieldFromValue = fromValue >=> (prettyValidate . CouponPaymentDate)
