{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.Nested2PartyIDSource where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 758
--   , fieldName = "Nested2PartyIDSource"
--   , fieldType = FieldTypeChar
--   , fieldValues = []
--   }
newtype Nested2PartyIDSource = Nested2PartyIDSource {unNested2PartyIDSource :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity Nested2PartyIDSource

instance IsField Nested2PartyIDSource where
  fieldTag Proxy = 758
  fieldIsData Proxy = False
  fieldToValue = toValue . unNested2PartyIDSource
  fieldFromValue = fromValue >=> (prettyValidate . Nested2PartyIDSource)