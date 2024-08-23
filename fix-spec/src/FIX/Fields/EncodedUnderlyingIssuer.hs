{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.EncodedUnderlyingIssuer where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 363, fieldName = "EncodedUnderlyingIssuer", fieldType = FieldTypeData, fieldValues = []}
newtype EncodedUnderlyingIssuer = EncodedUnderlyingIssuer {unEncodedUnderlyingIssuer :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity EncodedUnderlyingIssuer

instance IsField EncodedUnderlyingIssuer where
  fieldTag Proxy = 363
  fieldIsData Proxy = True
  fieldToValue = toValue . unEncodedUnderlyingIssuer
  fieldFromValue = fromValue >=> (prettyValidate . EncodedUnderlyingIssuer)
