{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.StartCash where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 921
--   , fieldName = "StartCash"
--   , fieldType = FieldTypeAMT
--   , fieldValues = []
--   }
newtype StartCash = StartCash {unStartCash :: Amount}
  deriving stock (Show, Eq, Generic)

instance Validity StartCash

instance IsField StartCash where
  fieldTag Proxy = 921
  fieldIsData Proxy = False
  fieldToValue = toValue . unStartCash
  fieldFromValue = fromValue >=> (prettyValidate . StartCash)
