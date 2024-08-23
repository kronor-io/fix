{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.BuyVolume where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 330, fieldName = "BuyVolume", fieldType = FieldTypeQTY, fieldValues = []}
newtype BuyVolume = BuyVolume {unBuyVolume :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity BuyVolume

instance IsField BuyVolume where
  fieldTag Proxy = 330
  fieldIsData Proxy = False
  fieldToValue = toValue . unBuyVolume
  fieldFromValue = fromValue >=> (prettyValidate . BuyVolume)
