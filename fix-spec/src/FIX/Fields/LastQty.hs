{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LastQty where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 32, fieldName = "LastQty", fieldType = FieldTypeQTY, fieldValues = []}
newtype LastQty = LastQty {unLastQty :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity LastQty

instance IsField LastQty where
  fieldTag Proxy = 32
  fieldIsData Proxy = False
  fieldToValue = toValue . unLastQty
  fieldFromValue = fromValue >=> (prettyValidate . LastQty)
