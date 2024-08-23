{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.OrigClOrdID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 41, fieldName = "OrigClOrdID", fieldType = FieldTypeString, fieldValues = []}
newtype OrigClOrdID = OrigClOrdID {unOrigClOrdID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity OrigClOrdID

instance IsField OrigClOrdID where
  fieldTag Proxy = 41
  fieldIsData Proxy = False
  fieldToValue = toValue . unOrigClOrdID
  fieldFromValue = fromValue >=> (prettyValidate . OrigClOrdID)
