{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.OfferForwardPoints where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 191, fieldName = "OfferForwardPoints", fieldType = FieldTypePriceOffset, fieldValues = []}
newtype OfferForwardPoints = OfferForwardPoints {unOfferForwardPoints :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity OfferForwardPoints

instance IsField OfferForwardPoints where
  fieldTag Proxy = 191
  fieldIsData Proxy = False
  fieldToValue = toValue . unOfferForwardPoints
  fieldFromValue = fromValue >=> (prettyValidate . OfferForwardPoints)
