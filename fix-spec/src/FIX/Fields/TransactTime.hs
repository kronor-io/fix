{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.TransactTime where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 60, fieldName = "TransactTime", fieldType = FieldTypeUTCTimestamp, fieldValues = []}
newtype TransactTime = TransactTime {unTransactTime :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity TransactTime

instance IsField TransactTime where
  fieldTag Proxy = 60
  fieldIsData Proxy = False
  fieldToValue = toValue . unTransactTime
  fieldFromValue = fromValue >=> (prettyValidate . TransactTime)
