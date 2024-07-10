{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.NoNested2PartySubIDs where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 806
--   , fieldName = "NoNested2PartySubIDs"
--   , fieldType = FieldTypeNumInGroup
--   , fieldValues = []
--   }
newtype NoNested2PartySubIDs = NoNested2PartySubIDs {unNoNested2PartySubIDs :: Word}
  deriving stock (Show, Eq, Generic)

instance Validity NoNested2PartySubIDs

instance IsField NoNested2PartySubIDs where
  fieldTag Proxy = 806
  fieldIsData Proxy = False
  fieldToValue = toValue . unNoNested2PartySubIDs
  fieldFromValue = fromValue >=> (prettyValidate . NoNested2PartySubIDs)
