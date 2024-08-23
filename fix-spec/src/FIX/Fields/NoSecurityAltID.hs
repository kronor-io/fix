{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.NoSecurityAltID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 454, fieldName = "NoSecurityAltID", fieldType = FieldTypeNumInGroup, fieldValues = []}
newtype NoSecurityAltID = NoSecurityAltID {unNoSecurityAltID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity NoSecurityAltID

instance IsField NoSecurityAltID where
  fieldTag Proxy = 454
  fieldIsData Proxy = False
  fieldToValue = toValue . unNoSecurityAltID
  fieldFromValue = fromValue >=> (prettyValidate . NoSecurityAltID)
