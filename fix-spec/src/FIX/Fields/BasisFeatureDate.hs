{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.BasisFeatureDate where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 259
--   , fieldName = "BasisFeatureDate"
--   , fieldType = FieldTypeLocalMktDate
--   , fieldValues = []
--   }
newtype BasisFeatureDate = BasisFeatureDate {unBasisFeatureDate :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity BasisFeatureDate

instance IsField BasisFeatureDate where
  fieldTag Proxy = 259
  fieldIsData Proxy = False
  fieldToValue = toValue . unBasisFeatureDate
  fieldFromValue = fromValue >=> (prettyValidate . BasisFeatureDate)
