{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.NoMsgTypes where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 384, fieldName = "NoMsgTypes", fieldType = FieldTypeNumInGroup, fieldValues = []}
newtype NoMsgTypes = NoMsgTypes {unNoMsgTypes :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity NoMsgTypes

instance IsField NoMsgTypes where
  fieldTag Proxy = 384
  fieldIsData Proxy = False
  fieldToValue = toValue . unNoMsgTypes
  fieldFromValue = fromValue >=> (prettyValidate . NoMsgTypes)
