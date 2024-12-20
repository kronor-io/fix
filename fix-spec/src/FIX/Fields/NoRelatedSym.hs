{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.NoRelatedSym where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 146
--   , fieldName = "NoRelatedSym"
--   , fieldType = FieldTypeNumInGroup
--   , fieldValues = []
--   }
newtype NoRelatedSym = NoRelatedSym {unNoRelatedSym :: Word}
  deriving stock (Show, Eq, Generic)

instance Validity NoRelatedSym

instance IsField NoRelatedSym where
  fieldTag Proxy = 146
  fieldIsData Proxy = False
  fieldToValue = toValue . unNoRelatedSym
  fieldFromValue = fromValue >=> (prettyValidate . NoRelatedSym)
