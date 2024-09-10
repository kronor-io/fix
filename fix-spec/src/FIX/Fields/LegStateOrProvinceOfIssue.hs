{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegStateOrProvinceOfIssue where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 597
--   , fieldName = "LegStateOrProvinceOfIssue"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype LegStateOrProvinceOfIssue = LegStateOrProvinceOfIssue {unLegStateOrProvinceOfIssue :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity LegStateOrProvinceOfIssue

instance IsField LegStateOrProvinceOfIssue where
  fieldTag Proxy = 597
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegStateOrProvinceOfIssue
  fieldFromValue = fromValue >=> (prettyValidate . LegStateOrProvinceOfIssue)
