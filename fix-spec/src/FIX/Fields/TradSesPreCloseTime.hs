{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.TradSesPreCloseTime where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 343, fieldName = "TradSesPreCloseTime", fieldType = FieldTypeUTCTimestamp, fieldValues = []}
newtype TradSesPreCloseTime = TradSesPreCloseTime {unTradSesPreCloseTime :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity TradSesPreCloseTime

instance IsField TradSesPreCloseTime where
  fieldTag Proxy = 343
  fieldIsData Proxy = False
  fieldToValue = toValue . unTradSesPreCloseTime
  fieldFromValue = fromValue >=> (prettyValidate . TradSesPreCloseTime)
