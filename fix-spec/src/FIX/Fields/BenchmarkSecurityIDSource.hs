{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.BenchmarkSecurityIDSource where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 761, fieldName = "BenchmarkSecurityIDSource", fieldType = FieldTypeString, fieldValues = []}
newtype BenchmarkSecurityIDSource = BenchmarkSecurityIDSource {unBenchmarkSecurityIDSource :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity BenchmarkSecurityIDSource

instance IsField BenchmarkSecurityIDSource where
  fieldTag Proxy = 761
  fieldIsData Proxy = False
  fieldToValue = toValue . unBenchmarkSecurityIDSource
  fieldFromValue = fromValue >=> (prettyValidate . BenchmarkSecurityIDSource)
