{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.Text where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 58
--   , fieldName = "Text"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype Text = Text {unText :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity Text

instance IsField Text where
  fieldTag Proxy = 58
  fieldIsData Proxy = False
  fieldToValue = toValue . unText
  fieldFromValue = fromValue >=> (prettyValidate . Text)
