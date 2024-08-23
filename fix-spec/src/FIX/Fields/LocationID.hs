{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LocationID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 283, fieldName = "LocationID", fieldType = FieldTypeString, fieldValues = []}
newtype LocationID = LocationID {unLocationID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity LocationID

instance IsField LocationID where
  fieldTag Proxy = 283
  fieldIsData Proxy = False
  fieldToValue = toValue . unLocationID
  fieldFromValue = fromValue >=> (prettyValidate . LocationID)
