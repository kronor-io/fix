{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.HeartBtInt where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 108, fieldName = "HeartBtInt", fieldType = FieldTypeInt, fieldValues = []}
newtype HeartBtInt = HeartBtInt {unHeartBtInt :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity HeartBtInt

instance IsField HeartBtInt where
  fieldTag Proxy = 108
  fieldIsData Proxy = False
  fieldToValue = toValue . unHeartBtInt
  fieldFromValue = fromValue >=> (prettyValidate . HeartBtInt)
