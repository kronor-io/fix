{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.RedemptionDate where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 240
--   , fieldName = "RedemptionDate"
--   , fieldType = FieldTypeLocalMktDate
--   , fieldValues = []
--   }
newtype RedemptionDate = RedemptionDate {unRedemptionDate :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity RedemptionDate

instance IsField RedemptionDate where
  fieldTag Proxy = 240
  fieldIsData Proxy = False
  fieldToValue = toValue . unRedemptionDate
  fieldFromValue = fromValue >=> (prettyValidate . RedemptionDate)
