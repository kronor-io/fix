{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.NoNested3PartyIDs where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 948, fieldName = "NoNested3PartyIDs", fieldType = FieldTypeNumInGroup, fieldValues = []}
newtype NoNested3PartyIDs = NoNested3PartyIDs {unNoNested3PartyIDs :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity NoNested3PartyIDs

instance IsField NoNested3PartyIDs where
  fieldTag Proxy = 948
  fieldIsData Proxy = False
  fieldToValue = toValue . unNoNested3PartyIDs
  fieldFromValue = fromValue >=> (prettyValidate . NoNested3PartyIDs)
