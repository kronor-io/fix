{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegStipulationValue where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 689, fieldName = "LegStipulationValue", fieldType = FieldTypeString, fieldValues = []}
newtype LegStipulationValue = LegStipulationValue {unLegStipulationValue :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity LegStipulationValue

instance IsField LegStipulationValue where
  fieldTag Proxy = 689
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegStipulationValue
  fieldFromValue = fromValue >=> (prettyValidate . LegStipulationValue)
