{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.ExpireDate where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 432
--   , fieldName = "ExpireDate"
--   , fieldType = FieldTypeLocalMktDate
--   , fieldValues = []
--   }
newtype ExpireDate = ExpireDate {unExpireDate :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity ExpireDate

instance IsField ExpireDate where
  fieldTag Proxy = 432
  fieldIsData Proxy = False
  fieldToValue = toValue . unExpireDate
  fieldFromValue = fromValue >=> (prettyValidate . ExpireDate)