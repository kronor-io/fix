{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.PriceImprovement where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 639, fieldName = "PriceImprovement", fieldType = FieldTypePriceOffset, fieldValues = []}
newtype PriceImprovement = PriceImprovement {unPriceImprovement :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity PriceImprovement

instance IsField PriceImprovement where
  fieldTag Proxy = 639
  fieldIsData Proxy = False
  fieldToValue = toValue . unPriceImprovement
  fieldFromValue = fromValue >=> (prettyValidate . PriceImprovement)
