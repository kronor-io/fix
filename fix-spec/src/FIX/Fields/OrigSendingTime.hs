{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.OrigSendingTime where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 122
--   , fieldName = "OrigSendingTime"
--   , fieldType = FieldTypeUTCTimestamp
--   , fieldValues = []
--   }
newtype OrigSendingTime = OrigSendingTime {unOrigSendingTime :: UTCTimestamp}
  deriving stock (Show, Eq, Generic)

instance Validity OrigSendingTime

instance IsField OrigSendingTime where
  fieldTag Proxy = 122
  fieldIsData Proxy = False
  fieldToValue = toValue . unOrigSendingTime
  fieldFromValue = fromValue >=> (prettyValidate . OrigSendingTime)