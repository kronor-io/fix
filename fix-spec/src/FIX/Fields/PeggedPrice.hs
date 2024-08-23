{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.PeggedPrice where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 839, fieldName = "PeggedPrice", fieldType = FieldTypePrice, fieldValues = []}
newtype PeggedPrice = PeggedPrice {unPeggedPrice :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity PeggedPrice

instance IsField PeggedPrice where
  fieldTag Proxy = 839
  fieldIsData Proxy = False
  fieldToValue = toValue . unPeggedPrice
  fieldFromValue = fromValue >=> (prettyValidate . PeggedPrice)
