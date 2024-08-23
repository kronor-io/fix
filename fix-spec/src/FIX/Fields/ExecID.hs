{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.ExecID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 17, fieldName = "ExecID", fieldType = FieldTypeString, fieldValues = []}
newtype ExecID = ExecID {unExecID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity ExecID

instance IsField ExecID where
  fieldTag Proxy = 17
  fieldIsData Proxy = False
  fieldToValue = toValue . unExecID
  fieldFromValue = fromValue >=> (prettyValidate . ExecID)
