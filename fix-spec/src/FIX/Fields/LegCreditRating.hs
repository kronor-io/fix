{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegCreditRating where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 257
--   , fieldName = "LegCreditRating"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype LegCreditRating = LegCreditRating {unLegCreditRating :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity LegCreditRating

instance IsField LegCreditRating where
  fieldTag Proxy = 257
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegCreditRating
  fieldFromValue = fromValue >=> (prettyValidate . LegCreditRating)