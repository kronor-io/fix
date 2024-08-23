{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.NoPositions where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 702, fieldName = "NoPositions", fieldType = FieldTypeNumInGroup, fieldValues = []}
newtype NoPositions = NoPositions {unNoPositions :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity NoPositions

instance IsField NoPositions where
  fieldTag Proxy = 702
  fieldIsData Proxy = False
  fieldToValue = toValue . unNoPositions
  fieldFromValue = fromValue >=> (prettyValidate . NoPositions)
