{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.NoLegAllocs where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 670
--   , fieldName = "NoLegAllocs"
--   , fieldType = FieldTypeNumInGroup
--   , fieldValues = []
--   }
newtype NoLegAllocs = NoLegAllocs {unNoLegAllocs :: Word}
  deriving stock (Show, Eq, Generic)

instance Validity NoLegAllocs

instance IsField NoLegAllocs where
  fieldTag Proxy = 670
  fieldIsData Proxy = False
  fieldToValue = toValue . unNoLegAllocs
  fieldFromValue = fromValue >=> (prettyValidate . NoLegAllocs)
