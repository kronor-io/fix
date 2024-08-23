{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.YieldRedemptionPrice where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 697, fieldName = "YieldRedemptionPrice", fieldType = FieldTypePrice, fieldValues = []}
newtype YieldRedemptionPrice = YieldRedemptionPrice {unYieldRedemptionPrice :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity YieldRedemptionPrice

instance IsField YieldRedemptionPrice where
  fieldTag Proxy = 697
  fieldIsData Proxy = False
  fieldToValue = toValue . unYieldRedemptionPrice
  fieldFromValue = fromValue >=> (prettyValidate . YieldRedemptionPrice)
