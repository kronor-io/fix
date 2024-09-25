{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegRepoCollateralSecurityType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 250
--   , fieldName = "LegRepoCollateralSecurityType"
--   , fieldType = FieldTypeInt
--   , fieldValues = []
--   }
newtype LegRepoCollateralSecurityType = LegRepoCollateralSecurityType {unLegRepoCollateralSecurityType :: Int}
  deriving stock (Show, Eq, Generic)

instance Validity LegRepoCollateralSecurityType

instance IsField LegRepoCollateralSecurityType where
  fieldTag Proxy = 250
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegRepoCollateralSecurityType
  fieldFromValue = fromValue >=> (prettyValidate . LegRepoCollateralSecurityType)