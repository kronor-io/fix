{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.Nested3PartyID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 949
--   , fieldName = "Nested3PartyID"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype Nested3PartyID = Nested3PartyID {unNested3PartyID :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity Nested3PartyID

instance IsField Nested3PartyID where
  fieldTag Proxy = 949
  fieldIsData Proxy = False
  fieldToValue = toValue . unNested3PartyID
  fieldFromValue = fromValue >=> (prettyValidate . Nested3PartyID)
