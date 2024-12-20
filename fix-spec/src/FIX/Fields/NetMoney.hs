{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.NetMoney where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 118
--   , fieldName = "NetMoney"
--   , fieldType = FieldTypeAMT
--   , fieldValues = []
--   }
newtype NetMoney = NetMoney {unNetMoney :: Amount}
  deriving stock (Show, Eq, Generic)

instance Validity NetMoney

instance IsField NetMoney where
  fieldTag Proxy = 118
  fieldIsData Proxy = False
  fieldToValue = toValue . unNetMoney
  fieldFromValue = fromValue >=> (prettyValidate . NetMoney)
