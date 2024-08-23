{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.NoStrikes where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 428, fieldName = "NoStrikes", fieldType = FieldTypeNumInGroup, fieldValues = []}
newtype NoStrikes = NoStrikes {unNoStrikes :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity NoStrikes

instance IsField NoStrikes where
  fieldTag Proxy = 428
  fieldIsData Proxy = False
  fieldToValue = toValue . unNoStrikes
  fieldFromValue = fromValue >=> (prettyValidate . NoStrikes)
