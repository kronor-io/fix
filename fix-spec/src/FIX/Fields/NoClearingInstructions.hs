{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.NoClearingInstructions where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 576, fieldName = "NoClearingInstructions", fieldType = FieldTypeNumInGroup, fieldValues = []}
newtype NoClearingInstructions = NoClearingInstructions {unNoClearingInstructions :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity NoClearingInstructions

instance IsField NoClearingInstructions where
  fieldTag Proxy = 576
  fieldIsData Proxy = False
  fieldToValue = toValue . unNoClearingInstructions
  fieldFromValue = fromValue >=> (prettyValidate . NoClearingInstructions)
