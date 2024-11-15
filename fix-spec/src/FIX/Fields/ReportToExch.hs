{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.ReportToExch where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 113
--   , fieldName = "ReportToExch"
--   , fieldType = FieldTypeBoolean
--   , fieldValues = []
--   }
newtype ReportToExch = ReportToExch {unReportToExch :: Bool}
  deriving stock (Show, Eq, Generic)

instance Validity ReportToExch

instance IsField ReportToExch where
  fieldTag Proxy = 113
  fieldIsData Proxy = False
  fieldToValue = toValue . unReportToExch
  fieldFromValue = fromValue >=> (prettyValidate . ReportToExch)
