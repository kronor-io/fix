{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegRedemptionDate where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 254, fieldName = "LegRedemptionDate", fieldType = FieldTypeLocalMktDate, fieldValues = []}
newtype LegRedemptionDate = LegRedemptionDate {unLegRedemptionDate :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity LegRedemptionDate

instance IsField LegRedemptionDate where
  fieldTag Proxy = 254
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegRedemptionDate
  fieldFromValue = fromValue >=> (prettyValidate . LegRedemptionDate)
