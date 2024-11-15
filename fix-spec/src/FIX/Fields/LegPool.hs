{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegPool where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 740
--   , fieldName = "LegPool"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype LegPool = LegPool {unLegPool :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity LegPool

instance IsField LegPool where
  fieldTag Proxy = 740
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegPool
  fieldFromValue = fromValue >=> (prettyValidate . LegPool)
