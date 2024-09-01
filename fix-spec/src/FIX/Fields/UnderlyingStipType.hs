{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.UnderlyingStipType where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 888
--   , fieldName = "UnderlyingStipType"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype UnderlyingStipType = UnderlyingStipType {unUnderlyingStipType :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity UnderlyingStipType

instance IsField UnderlyingStipType where
  fieldTag Proxy = 888
  fieldIsData Proxy = False
  fieldToValue = toValue . unUnderlyingStipType
  fieldFromValue = fromValue >=> (prettyValidate . UnderlyingStipType)