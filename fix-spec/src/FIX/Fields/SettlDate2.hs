{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SettlDate2 where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 193, fieldName = "SettlDate2", fieldType = FieldTypeLocalMktDate, fieldValues = []}
newtype SettlDate2 = SettlDate2 {unSettlDate2 :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity SettlDate2

instance IsField SettlDate2 where
  fieldTag Proxy = 193
  fieldIsData Proxy = False
  fieldToValue = toValue . unSettlDate2
  fieldFromValue = fromValue >=> (prettyValidate . SettlDate2)
