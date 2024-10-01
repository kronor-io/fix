{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.Concession where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 238
--   , fieldName = "Concession"
--   , fieldType = FieldTypeAMT
--   , fieldValues = []
--   }
newtype Concession = Concession {unConcession :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity Concession

instance IsField Concession where
  fieldTag Proxy = 238
  fieldIsData Proxy = False
  fieldToValue = toValue . unConcession
  fieldFromValue = fromValue >=> (prettyValidate . Concession)