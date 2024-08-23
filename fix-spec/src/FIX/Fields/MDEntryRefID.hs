{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.MDEntryRefID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 280, fieldName = "MDEntryRefID", fieldType = FieldTypeString, fieldValues = []}
newtype MDEntryRefID = MDEntryRefID {unMDEntryRefID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity MDEntryRefID

instance IsField MDEntryRefID where
  fieldTag Proxy = 280
  fieldIsData Proxy = False
  fieldToValue = toValue . unMDEntryRefID
  fieldFromValue = fromValue >=> (prettyValidate . MDEntryRefID)
