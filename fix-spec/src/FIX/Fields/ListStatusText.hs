{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.ListStatusText where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 444, fieldName = "ListStatusText", fieldType = FieldTypeString, fieldValues = []}
newtype ListStatusText = ListStatusText {unListStatusText :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity ListStatusText

instance IsField ListStatusText where
  fieldTag Proxy = 444
  fieldIsData Proxy = False
  fieldToValue = toValue . unListStatusText
  fieldFromValue = fromValue >=> (prettyValidate . ListStatusText)
