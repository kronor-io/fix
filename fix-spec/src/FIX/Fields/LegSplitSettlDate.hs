{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegSplitSettlDate where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 14105
--   , fieldName = "LegSplitSettlDate"
--   , fieldType = FieldTypeLocalMktDate
--   , fieldValues = []
--   }
newtype LegSplitSettlDate = LegSplitSettlDate {unLegSplitSettlDate :: LocalMktDate}
  deriving stock (Show, Eq, Generic)

instance Validity LegSplitSettlDate

instance IsField LegSplitSettlDate where
  fieldTag Proxy = 14105
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegSplitSettlDate
  fieldFromValue = fromValue >=> (prettyValidate . LegSplitSettlDate)
