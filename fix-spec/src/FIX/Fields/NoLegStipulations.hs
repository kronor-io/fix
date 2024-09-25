{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.NoLegStipulations where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 683
--   , fieldName = "NoLegStipulations"
--   , fieldType = FieldTypeNumInGroup
--   , fieldValues = []
--   }
newtype NoLegStipulations = NoLegStipulations {unNoLegStipulations :: Word}
  deriving stock (Show, Eq, Generic)

instance Validity NoLegStipulations

instance IsField NoLegStipulations where
  fieldTag Proxy = 683
  fieldIsData Proxy = False
  fieldToValue = toValue . unNoLegStipulations
  fieldFromValue = fromValue >=> (prettyValidate . NoLegStipulations)