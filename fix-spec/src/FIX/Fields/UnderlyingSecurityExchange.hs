{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.UnderlyingSecurityExchange where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 308, fieldName = "UnderlyingSecurityExchange", fieldType = FieldTypeExchange, fieldValues = []}
newtype UnderlyingSecurityExchange = UnderlyingSecurityExchange {unUnderlyingSecurityExchange :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity UnderlyingSecurityExchange

instance IsField UnderlyingSecurityExchange where
  fieldTag Proxy = 308
  fieldIsData Proxy = False
  fieldToValue = toValue . unUnderlyingSecurityExchange
  fieldFromValue = fromValue >=> (prettyValidate . UnderlyingSecurityExchange)
