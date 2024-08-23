{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SellVolume where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 331, fieldName = "SellVolume", fieldType = FieldTypeQTY, fieldValues = []}
newtype SellVolume = SellVolume {unSellVolume :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity SellVolume

instance IsField SellVolume where
  fieldTag Proxy = 331
  fieldIsData Proxy = False
  fieldToValue = toValue . unSellVolume
  fieldFromValue = fromValue >=> (prettyValidate . SellVolume)
