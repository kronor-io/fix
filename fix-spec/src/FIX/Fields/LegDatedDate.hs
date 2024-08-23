{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegDatedDate where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 739, fieldName = "LegDatedDate", fieldType = FieldTypeLocalMktDate, fieldValues = []}
newtype LegDatedDate = LegDatedDate {unLegDatedDate :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity LegDatedDate

instance IsField LegDatedDate where
  fieldTag Proxy = 739
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegDatedDate
  fieldFromValue = fromValue >=> (prettyValidate . LegDatedDate)
