{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.CopyMsgIndicator where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 797, fieldName = "CopyMsgIndicator", fieldType = FieldTypeBoolean, fieldValues = []}
newtype CopyMsgIndicator = CopyMsgIndicator {unCopyMsgIndicator :: Bool}
  deriving stock (Show, Eq, Generic)

instance Validity CopyMsgIndicator

instance IsField CopyMsgIndicator where
  fieldTag Proxy = 797
  fieldIsData Proxy = False
  fieldToValue = toValue . unCopyMsgIndicator
  fieldFromValue = fromValue >=> (prettyValidate . CopyMsgIndicator)
