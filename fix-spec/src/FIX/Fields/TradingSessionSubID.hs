{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.TradingSessionSubID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 625
--   , fieldName = "TradingSessionSubID"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype TradingSessionSubID = TradingSessionSubID {unTradingSessionSubID :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity TradingSessionSubID

instance IsField TradingSessionSubID where
  fieldTag Proxy = 625
  fieldIsData Proxy = False
  fieldToValue = toValue . unTradingSessionSubID
  fieldFromValue = fromValue >=> (prettyValidate . TradingSessionSubID)
