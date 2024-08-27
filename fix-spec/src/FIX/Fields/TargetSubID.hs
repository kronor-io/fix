{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.TargetSubID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec {fieldNumber = 57, fieldName = "TargetSubID", fieldType = FieldTypeString, fieldValues = []}
newtype TargetSubID = TargetSubID {unTargetSubID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity TargetSubID

instance IsField TargetSubID where
  fieldTag Proxy = 57
  fieldIsData Proxy = False
  fieldToValue = toValue . unTargetSubID
  fieldFromValue = fromValue >=> (prettyValidate . TargetSubID)