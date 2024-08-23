{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.RefTagID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 371, fieldName = "RefTagID", fieldType = FieldTypeInt, fieldValues = []}
newtype RefTagID = RefTagID {unRefTagID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity RefTagID

instance IsField RefTagID where
  fieldTag Proxy = 371
  fieldIsData Proxy = False
  fieldToValue = toValue . unRefTagID
  fieldFromValue = fromValue >=> (prettyValidate . RefTagID)
