{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.UnderlyingLastPx where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 651
--   , fieldName = "UnderlyingLastPx"
--   , fieldType = FieldTypePrice
--   , fieldValues = []
--   }
newtype UnderlyingLastPx = UnderlyingLastPx {unUnderlyingLastPx :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity UnderlyingLastPx

instance IsField UnderlyingLastPx where
  fieldTag Proxy = 651
  fieldIsData Proxy = False
  fieldToValue = toValue . unUnderlyingLastPx
  fieldFromValue = fromValue >=> (prettyValidate . UnderlyingLastPx)