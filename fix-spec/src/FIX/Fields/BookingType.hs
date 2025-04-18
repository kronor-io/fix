{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.BookingType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 775
--   , fieldName = "BookingType"
--   , fieldType = FieldTypeInt
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "0"
--           , fieldValueDescription = "REGULAR_BOOKING"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "1" , fieldValueDescription = "CFD" }
--       , FieldValueSpec
--           { fieldValueEnum = "2"
--           , fieldValueDescription = "TOTAL_RETURN_SWAP"
--           }
--       ]
--   }
data BookingType
  = BookingTypeRegularBooking
  | BookingTypeCfd
  | BookingTypeTotalReturnSwap
  deriving stock (Show, Eq, Generic)

instance Validity BookingType

instance IsField BookingType where
  fieldTag Proxy = 775
  fieldIsData Proxy = False
  fieldToValue = \case
    BookingTypeRegularBooking -> "0"
    BookingTypeCfd -> "1"
    BookingTypeTotalReturnSwap -> "2"
  fieldFromValue = \case
    "0" -> Right BookingTypeRegularBooking
    "1" -> Right BookingTypeCfd
    "2" -> Right BookingTypeTotalReturnSwap
    v -> Left ("Unknown BookingType: " <> show v)
