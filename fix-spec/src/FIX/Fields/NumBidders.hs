{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.NumBidders where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 417, fieldName = "NumBidders", fieldType = FieldTypeInt, fieldValues = []}
newtype NumBidders = NumBidders {unNumBidders :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity NumBidders

instance IsField NumBidders where
  fieldTag Proxy = 417
  fieldIsData Proxy = False
  fieldToValue = toValue . unNumBidders
  fieldFromValue = fromValue >=> (prettyValidate . NumBidders)
