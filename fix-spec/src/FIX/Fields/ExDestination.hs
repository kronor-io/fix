{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.ExDestination where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 100
--   , fieldName = "ExDestination"
--   , fieldType = FieldTypeExchange
--   , fieldValues = []
--   }
newtype ExDestination = ExDestination {unExDestination :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity ExDestination

instance IsField ExDestination where
  fieldTag Proxy = 100
  fieldIsData Proxy = False
  fieldToValue = toValue . unExDestination
  fieldFromValue = fromValue >=> (prettyValidate . ExDestination)
