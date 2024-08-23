{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.BenchmarkCurvePoint where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 222, fieldName = "BenchmarkCurvePoint", fieldType = FieldTypeString, fieldValues = []}
newtype BenchmarkCurvePoint = BenchmarkCurvePoint {unBenchmarkCurvePoint :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity BenchmarkCurvePoint

instance IsField BenchmarkCurvePoint where
  fieldTag Proxy = 222
  fieldIsData Proxy = False
  fieldToValue = toValue . unBenchmarkCurvePoint
  fieldFromValue = fromValue >=> (prettyValidate . BenchmarkCurvePoint)
