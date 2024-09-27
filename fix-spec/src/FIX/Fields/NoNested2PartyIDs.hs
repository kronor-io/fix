{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.NoNested2PartyIDs where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 756
--   , fieldName = "NoNested2PartyIDs"
--   , fieldType = FieldTypeNumInGroup
--   , fieldValues = []
--   }
newtype NoNested2PartyIDs = NoNested2PartyIDs {unNoNested2PartyIDs :: Word}
  deriving stock (Show, Eq, Generic)

instance Validity NoNested2PartyIDs

instance IsField NoNested2PartyIDs where
  fieldTag Proxy = 756
  fieldIsData Proxy = False
  fieldToValue = toValue . unNoNested2PartyIDs
  fieldFromValue = fromValue >=> (prettyValidate . NoNested2PartyIDs)
