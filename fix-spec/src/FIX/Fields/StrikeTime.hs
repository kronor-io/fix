{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.StrikeTime where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 443, fieldName = "StrikeTime", fieldType = FieldTypeUTCTimestamp, fieldValues = []}
newtype StrikeTime = StrikeTime {unStrikeTime :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity StrikeTime

instance IsField StrikeTime where
  fieldTag Proxy = 443
  fieldIsData Proxy = False
  fieldToValue = toValue . unStrikeTime
  fieldFromValue = fromValue >=> (prettyValidate . StrikeTime)
