{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.OnBehalfOfLocationID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 144, fieldName = "OnBehalfOfLocationID", fieldType = FieldTypeString, fieldValues = []}
newtype OnBehalfOfLocationID = OnBehalfOfLocationID {unOnBehalfOfLocationID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity OnBehalfOfLocationID

instance IsField OnBehalfOfLocationID where
  fieldTag Proxy = 144
  fieldIsData Proxy = False
  fieldToValue = toValue . unOnBehalfOfLocationID
  fieldFromValue = fromValue >=> (prettyValidate . OnBehalfOfLocationID)
