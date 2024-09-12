{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.NoLinesOfText where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 33
--   , fieldName = "NoLinesOfText"
--   , fieldType = FieldTypeNumInGroup
--   , fieldValues = []
--   }
newtype NoLinesOfText = NoLinesOfText {unNoLinesOfText :: Word}
  deriving stock (Show, Eq, Generic)

instance Validity NoLinesOfText

instance IsField NoLinesOfText where
  fieldTag Proxy = 33
  fieldIsData Proxy = False
  fieldToValue = toValue . unNoLinesOfText
  fieldFromValue = fromValue >=> (prettyValidate . NoLinesOfText)