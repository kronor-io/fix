{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.EffectiveTime where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 168
--   , fieldName = "EffectiveTime"
--   , fieldType = FieldTypeUTCTimestamp
--   , fieldValues = []
--   }
newtype EffectiveTime = EffectiveTime {unEffectiveTime :: UTCTimestamp}
  deriving stock (Show, Eq, Generic)

instance Validity EffectiveTime

instance IsField EffectiveTime where
  fieldTag Proxy = 168
  fieldIsData Proxy = False
  fieldToValue = toValue . unEffectiveTime
  fieldFromValue = fromValue >=> (prettyValidate . EffectiveTime)
