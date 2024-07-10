{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegAllocAccount where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 671
--   , fieldName = "LegAllocAccount"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype LegAllocAccount = LegAllocAccount {unLegAllocAccount :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity LegAllocAccount

instance IsField LegAllocAccount where
  fieldTag Proxy = 671
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegAllocAccount
  fieldFromValue = fromValue >=> (prettyValidate . LegAllocAccount)
