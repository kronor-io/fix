{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.NoRoutingIDs where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 215, fieldName = "NoRoutingIDs", fieldType = FieldTypeNumInGroup, fieldValues = []}
newtype NoRoutingIDs = NoRoutingIDs {unNoRoutingIDs :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity NoRoutingIDs

instance IsField NoRoutingIDs where
  fieldTag Proxy = 215
  fieldIsData Proxy = False
  fieldToValue = toValue . unNoRoutingIDs
  fieldFromValue = fromValue >=> (prettyValidate . NoRoutingIDs)
