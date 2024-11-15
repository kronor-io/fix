{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.Headline where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 148
--   , fieldName = "Headline"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype Headline = Headline {unHeadline :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity Headline

instance IsField Headline where
  fieldTag Proxy = 148
  fieldIsData Proxy = False
  fieldToValue = toValue . unHeadline
  fieldFromValue = fromValue >=> (prettyValidate . Headline)
