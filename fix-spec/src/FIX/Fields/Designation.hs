{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.Designation where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 494
--   , fieldName = "Designation"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype Designation = Designation {unDesignation :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity Designation

instance IsField Designation where
  fieldTag Proxy = 494
  fieldIsData Proxy = False
  fieldToValue = toValue . unDesignation
  fieldFromValue = fromValue >=> (prettyValidate . Designation)