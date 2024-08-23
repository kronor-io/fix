{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegBenchmarkCurveName where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 677, fieldName = "LegBenchmarkCurveName", fieldType = FieldTypeString, fieldValues = []}
newtype LegBenchmarkCurveName = LegBenchmarkCurveName {unLegBenchmarkCurveName :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity LegBenchmarkCurveName

instance IsField LegBenchmarkCurveName where
  fieldTag Proxy = 677
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegBenchmarkCurveName
  fieldFromValue = fromValue >=> (prettyValidate . LegBenchmarkCurveName)
