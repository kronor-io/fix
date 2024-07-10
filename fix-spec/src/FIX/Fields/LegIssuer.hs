{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegIssuer where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 617
--   , fieldName = "LegIssuer"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype LegIssuer = LegIssuer {unLegIssuer :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity LegIssuer

instance IsField LegIssuer where
  fieldTag Proxy = 617
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegIssuer
  fieldFromValue = fromValue >=> (prettyValidate . LegIssuer)
