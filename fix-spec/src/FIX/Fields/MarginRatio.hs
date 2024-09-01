{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.MarginRatio where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 898
--   , fieldName = "MarginRatio"
--   , fieldType = FieldTypePercentage
--   , fieldValues = []
--   }
newtype MarginRatio = MarginRatio {unMarginRatio :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity MarginRatio

instance IsField MarginRatio where
  fieldTag Proxy = 898
  fieldIsData Proxy = False
  fieldToValue = toValue . unMarginRatio
  fieldFromValue = fromValue >=> (prettyValidate . MarginRatio)