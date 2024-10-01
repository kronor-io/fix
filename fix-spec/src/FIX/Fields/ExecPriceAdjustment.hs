{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.ExecPriceAdjustment where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 485
--   , fieldName = "ExecPriceAdjustment"
--   , fieldType = FieldTypeFloat
--   , fieldValues = []
--   }
newtype ExecPriceAdjustment = ExecPriceAdjustment {unExecPriceAdjustment :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity ExecPriceAdjustment

instance IsField ExecPriceAdjustment where
  fieldTag Proxy = 485
  fieldIsData Proxy = False
  fieldToValue = toValue . unExecPriceAdjustment
  fieldFromValue = fromValue >=> (prettyValidate . ExecPriceAdjustment)