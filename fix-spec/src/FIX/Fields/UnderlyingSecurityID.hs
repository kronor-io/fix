{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.UnderlyingSecurityID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 309, fieldName = "UnderlyingSecurityID", fieldType = FieldTypeString, fieldValues = []}
newtype UnderlyingSecurityID = UnderlyingSecurityID {unUnderlyingSecurityID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity UnderlyingSecurityID

instance IsField UnderlyingSecurityID where
  fieldTag Proxy = 309
  fieldIsData Proxy = False
  fieldToValue = toValue . unUnderlyingSecurityID
  fieldFromValue = fromValue >=> (prettyValidate . UnderlyingSecurityID)
