{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.MidPx where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 631
--   , fieldName = "MidPx"
--   , fieldType = FieldTypePrice
--   , fieldValues = []
--   }
newtype MidPx = MidPx {unMidPx :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity MidPx

instance IsField MidPx where
  fieldTag Proxy = 631
  fieldIsData Proxy = False
  fieldToValue = toValue . unMidPx
  fieldFromValue = fromValue >=> (prettyValidate . MidPx)