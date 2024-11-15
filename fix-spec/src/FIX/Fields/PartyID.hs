{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.PartyID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 448
--   , fieldName = "PartyID"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype PartyID = PartyID {unPartyID :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity PartyID

instance IsField PartyID where
  fieldTag Proxy = 448
  fieldIsData Proxy = False
  fieldToValue = toValue . unPartyID
  fieldFromValue = fromValue >=> (prettyValidate . PartyID)
