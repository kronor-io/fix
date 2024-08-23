{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.Nested3PartySubID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 953, fieldName = "Nested3PartySubID", fieldType = FieldTypeString, fieldValues = []}
newtype Nested3PartySubID = Nested3PartySubID {unNested3PartySubID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity Nested3PartySubID

instance IsField Nested3PartySubID where
  fieldTag Proxy = 953
  fieldIsData Proxy = False
  fieldToValue = toValue . unNested3PartySubID
  fieldFromValue = fromValue >=> (prettyValidate . Nested3PartySubID)
