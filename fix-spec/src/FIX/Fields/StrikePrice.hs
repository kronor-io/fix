{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.StrikePrice where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 202, fieldName = "StrikePrice", fieldType = FieldTypePrice, fieldValues = []}
newtype StrikePrice = StrikePrice {unStrikePrice :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity StrikePrice

instance IsField StrikePrice where
  fieldTag Proxy = 202
  fieldIsData Proxy = False
  fieldToValue = toValue . unStrikePrice
  fieldFromValue = fromValue >=> (prettyValidate . StrikePrice)
