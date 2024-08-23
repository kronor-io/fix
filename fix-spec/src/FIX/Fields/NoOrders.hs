{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.NoOrders where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 73, fieldName = "NoOrders", fieldType = FieldTypeNumInGroup, fieldValues = []}
newtype NoOrders = NoOrders {unNoOrders :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity NoOrders

instance IsField NoOrders where
  fieldTag Proxy = 73
  fieldIsData Proxy = False
  fieldToValue = toValue . unNoOrders
  fieldFromValue = fromValue >=> (prettyValidate . NoOrders)
