{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.CreditRating where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 255
--   , fieldName = "CreditRating"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype CreditRating = CreditRating {unCreditRating :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity CreditRating

instance IsField CreditRating where
  fieldTag Proxy = 255
  fieldIsData Proxy = False
  fieldToValue = toValue . unCreditRating
  fieldFromValue = fromValue >=> (prettyValidate . CreditRating)
