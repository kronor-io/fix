{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.ReversalIndicator where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 700, fieldName = "ReversalIndicator", fieldType = FieldTypeBoolean, fieldValues = []}
newtype ReversalIndicator = ReversalIndicator {unReversalIndicator :: Bool}
  deriving stock (Show, Eq, Generic)

instance Validity ReversalIndicator

instance IsField ReversalIndicator where
  fieldTag Proxy = 700
  fieldIsData Proxy = False
  fieldToValue = toValue . unReversalIndicator
  fieldFromValue = fromValue >=> (prettyValidate . ReversalIndicator)
