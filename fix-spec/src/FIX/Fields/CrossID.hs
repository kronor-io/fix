{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.CrossID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 548, fieldName = "CrossID", fieldType = FieldTypeString, fieldValues = []}
newtype CrossID = CrossID {unCrossID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity CrossID

instance IsField CrossID where
  fieldTag Proxy = 548
  fieldIsData Proxy = False
  fieldToValue = toValue . unCrossID
  fieldFromValue = fromValue >=> (prettyValidate . CrossID)
