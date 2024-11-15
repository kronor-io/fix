{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.TotNumReports where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 911
--   , fieldName = "TotNumReports"
--   , fieldType = FieldTypeInt
--   , fieldValues = []
--   }
newtype TotNumReports = TotNumReports {unTotNumReports :: Int}
  deriving stock (Show, Eq, Generic)

instance Validity TotNumReports

instance IsField TotNumReports where
  fieldTag Proxy = 911
  fieldIsData Proxy = False
  fieldToValue = toValue . unTotNumReports
  fieldFromValue = fromValue >=> (prettyValidate . TotNumReports)
