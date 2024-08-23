{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegBidPx where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 681, fieldName = "LegBidPx", fieldType = FieldTypePrice, fieldValues = []}
newtype LegBidPx = LegBidPx {unLegBidPx :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity LegBidPx

instance IsField LegBidPx where
  fieldTag Proxy = 681
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegBidPx
  fieldFromValue = fromValue >=> (prettyValidate . LegBidPx)
