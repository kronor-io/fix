{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.OrderQty where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 38
--   , fieldName = "OrderQty"
--   , fieldType = FieldTypeQTY
--   , fieldValues = []
--   }
newtype OrderQty = OrderQty {unOrderQty :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity OrderQty

instance IsField OrderQty where
  fieldTag Proxy = 38
  fieldIsData Proxy = False
  fieldToValue = toValue . unOrderQty
  fieldFromValue = fromValue >=> (prettyValidate . OrderQty)