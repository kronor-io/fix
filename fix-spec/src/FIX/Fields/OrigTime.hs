{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.OrigTime where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 42, fieldName = "OrigTime", fieldType = FieldTypeUTCTimestamp, fieldValues = []}
newtype OrigTime = OrigTime {unOrigTime :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity OrigTime

instance IsField OrigTime where
  fieldTag Proxy = 42
  fieldIsData Proxy = False
  fieldToValue = toValue . unOrigTime
  fieldFromValue = fromValue >=> (prettyValidate . OrigTime)
