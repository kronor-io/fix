{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.OrderQty2 where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 192
--   , fieldName = "OrderQty2"
--   , fieldType = FieldTypeQTY
--   , fieldValues = []
--   }
newtype OrderQty2 = OrderQty2 {unOrderQty2 :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity OrderQty2

instance IsField OrderQty2 where
  fieldTag Proxy = 192
  fieldIsData Proxy = False
  fieldToValue = toValue . unOrderQty2
  fieldFromValue = fromValue >=> (prettyValidate . OrderQty2)