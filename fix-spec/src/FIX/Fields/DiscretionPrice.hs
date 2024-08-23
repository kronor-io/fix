{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.DiscretionPrice where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 845, fieldName = "DiscretionPrice", fieldType = FieldTypePrice, fieldValues = []}
newtype DiscretionPrice = DiscretionPrice {unDiscretionPrice :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity DiscretionPrice

instance IsField DiscretionPrice where
  fieldTag Proxy = 845
  fieldIsData Proxy = False
  fieldToValue = toValue . unDiscretionPrice
  fieldFromValue = fromValue >=> (prettyValidate . DiscretionPrice)
