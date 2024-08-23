{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.DeliverToCompID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 128, fieldName = "DeliverToCompID", fieldType = FieldTypeString, fieldValues = []}
newtype DeliverToCompID = DeliverToCompID {unDeliverToCompID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity DeliverToCompID

instance IsField DeliverToCompID where
  fieldTag Proxy = 128
  fieldIsData Proxy = False
  fieldToValue = toValue . unDeliverToCompID
  fieldFromValue = fromValue >=> (prettyValidate . DeliverToCompID)
