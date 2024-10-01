{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.Nested3PartyIDSource where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 950
--   , fieldName = "Nested3PartyIDSource"
--   , fieldType = FieldTypeChar
--   , fieldValues = []
--   }
newtype Nested3PartyIDSource = Nested3PartyIDSource {unNested3PartyIDSource :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity Nested3PartyIDSource

instance IsField Nested3PartyIDSource where
  fieldTag Proxy = 950
  fieldIsData Proxy = False
  fieldToValue = toValue . unNested3PartyIDSource
  fieldFromValue = fromValue >=> (prettyValidate . Nested3PartyIDSource)