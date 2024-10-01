{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.Nested2PartyRole where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 759
--   , fieldName = "Nested2PartyRole"
--   , fieldType = FieldTypeInt
--   , fieldValues = []
--   }
newtype Nested2PartyRole = Nested2PartyRole {unNested2PartyRole :: Int}
  deriving stock (Show, Eq, Generic)

instance Validity Nested2PartyRole

instance IsField Nested2PartyRole where
  fieldTag Proxy = 759
  fieldIsData Proxy = False
  fieldToValue = toValue . unNested2PartyRole
  fieldFromValue = fromValue >=> (prettyValidate . Nested2PartyRole)