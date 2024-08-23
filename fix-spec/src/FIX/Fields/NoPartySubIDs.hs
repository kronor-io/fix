{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.NoPartySubIDs where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 802, fieldName = "NoPartySubIDs", fieldType = FieldTypeNumInGroup, fieldValues = []}
newtype NoPartySubIDs = NoPartySubIDs {unNoPartySubIDs :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity NoPartySubIDs

instance IsField NoPartySubIDs where
  fieldTag Proxy = 802
  fieldIsData Proxy = False
  fieldToValue = toValue . unNoPartySubIDs
  fieldFromValue = fromValue >=> (prettyValidate . NoPartySubIDs)
