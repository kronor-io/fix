{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LastSpotRate where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 194, fieldName = "LastSpotRate", fieldType = FieldTypePrice, fieldValues = []}
newtype LastSpotRate = LastSpotRate {unLastSpotRate :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity LastSpotRate

instance IsField LastSpotRate where
  fieldTag Proxy = 194
  fieldIsData Proxy = False
  fieldToValue = toValue . unLastSpotRate
  fieldFromValue = fromValue >=> (prettyValidate . LastSpotRate)
