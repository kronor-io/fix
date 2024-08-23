{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.ProgPeriodInterval where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 415, fieldName = "ProgPeriodInterval", fieldType = FieldTypeInt, fieldValues = []}
newtype ProgPeriodInterval = ProgPeriodInterval {unProgPeriodInterval :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity ProgPeriodInterval

instance IsField ProgPeriodInterval where
  fieldTag Proxy = 415
  fieldIsData Proxy = False
  fieldToValue = toValue . unProgPeriodInterval
  fieldFromValue = fromValue >=> (prettyValidate . ProgPeriodInterval)
