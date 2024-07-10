{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.YieldCalcDate where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 701
--   , fieldName = "YieldCalcDate"
--   , fieldType = FieldTypeLocalMktDate
--   , fieldValues = []
--   }
newtype YieldCalcDate = YieldCalcDate {unYieldCalcDate :: LocalMktDate}
  deriving stock (Show, Eq, Generic)

instance Validity YieldCalcDate

instance IsField YieldCalcDate where
  fieldTag Proxy = 701
  fieldIsData Proxy = False
  fieldToValue = toValue . unYieldCalcDate
  fieldFromValue = fromValue >=> (prettyValidate . YieldCalcDate)
