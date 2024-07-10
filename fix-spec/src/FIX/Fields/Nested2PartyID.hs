{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.Nested2PartyID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 757
--   , fieldName = "Nested2PartyID"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype Nested2PartyID = Nested2PartyID {unNested2PartyID :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity Nested2PartyID

instance IsField Nested2PartyID where
  fieldTag Proxy = 757
  fieldIsData Proxy = False
  fieldToValue = toValue . unNested2PartyID
  fieldFromValue = fromValue >=> (prettyValidate . Nested2PartyID)
