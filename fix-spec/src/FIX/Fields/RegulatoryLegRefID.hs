{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.RegulatoryLegRefID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 2411
--   , fieldName = "RegulatoryLegRefID"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype RegulatoryLegRefID = RegulatoryLegRefID {unRegulatoryLegRefID :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity RegulatoryLegRefID

instance IsField RegulatoryLegRefID where
  fieldTag Proxy = 2411
  fieldIsData Proxy = False
  fieldToValue = toValue . unRegulatoryLegRefID
  fieldFromValue = fromValue >=> (prettyValidate . RegulatoryLegRefID)
