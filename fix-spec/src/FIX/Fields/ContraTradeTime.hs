{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.ContraTradeTime where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 438
--   , fieldName = "ContraTradeTime"
--   , fieldType = FieldTypeUTCTimestamp
--   , fieldValues = []
--   }
newtype ContraTradeTime = ContraTradeTime {unContraTradeTime :: UTCTimestamp}
  deriving stock (Show, Eq, Generic)

instance Validity ContraTradeTime

instance IsField ContraTradeTime where
  fieldTag Proxy = 438
  fieldIsData Proxy = False
  fieldToValue = toValue . unContraTradeTime
  fieldFromValue = fromValue >=> (prettyValidate . ContraTradeTime)