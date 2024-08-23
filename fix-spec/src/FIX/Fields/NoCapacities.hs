{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.NoCapacities where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 862, fieldName = "NoCapacities", fieldType = FieldTypeNumInGroup, fieldValues = []}
newtype NoCapacities = NoCapacities {unNoCapacities :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity NoCapacities

instance IsField NoCapacities where
  fieldTag Proxy = 862
  fieldIsData Proxy = False
  fieldToValue = toValue . unNoCapacities
  fieldFromValue = fromValue >=> (prettyValidate . NoCapacities)
