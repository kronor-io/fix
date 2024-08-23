{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.OrderBookingQty where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 800, fieldName = "OrderBookingQty", fieldType = FieldTypeQTY, fieldValues = []}
newtype OrderBookingQty = OrderBookingQty {unOrderBookingQty :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity OrderBookingQty

instance IsField OrderBookingQty where
  fieldTag Proxy = 800
  fieldIsData Proxy = False
  fieldToValue = toValue . unOrderBookingQty
  fieldFromValue = fromValue >=> (prettyValidate . OrderBookingQty)
