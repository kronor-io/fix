{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.MiscFeeAmt where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 137
--   , fieldName = "MiscFeeAmt"
--   , fieldType = FieldTypeAMT
--   , fieldValues = []
--   }
newtype MiscFeeAmt = MiscFeeAmt {unMiscFeeAmt :: Amount}
  deriving stock (Show, Eq, Generic)

instance Validity MiscFeeAmt

instance IsField MiscFeeAmt where
  fieldTag Proxy = 137
  fieldIsData Proxy = False
  fieldToValue = toValue . unMiscFeeAmt
  fieldFromValue = fromValue >=> (prettyValidate . MiscFeeAmt)
