{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SendingTime where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec {fieldNumber = 52, fieldName = "SendingTime", fieldType = FieldTypeUTCTimestamp, fieldValues = []}
newtype SendingTime = SendingTime {unSendingTime :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity SendingTime

instance IsField SendingTime where
  fieldTag Proxy = 52
  fieldIsData Proxy = False
  fieldToValue = toValue . unSendingTime
  fieldFromValue = fromValue >=> (prettyValidate . SendingTime)
