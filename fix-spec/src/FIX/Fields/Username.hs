{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.Username where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec {fieldNumber = 553, fieldName = "Username", fieldType = FieldTypeString, fieldValues = []}
newtype Username = Username {unUsername :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity Username

instance IsField Username where
  fieldTag Proxy = 553
  fieldIsData Proxy = False
  fieldToValue = toValue . unUsername
  fieldFromValue = fromValue >=> (prettyValidate . Username)