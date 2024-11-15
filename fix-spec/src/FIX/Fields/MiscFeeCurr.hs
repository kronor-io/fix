{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.MiscFeeCurr where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 138
--   , fieldName = "MiscFeeCurr"
--   , fieldType = FieldTypeCurrency
--   , fieldValues = []
--   }
newtype MiscFeeCurr = MiscFeeCurr {unMiscFeeCurr :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity MiscFeeCurr

instance IsField MiscFeeCurr where
  fieldTag Proxy = 138
  fieldIsData Proxy = False
  fieldToValue = toValue . unMiscFeeCurr
  fieldFromValue = fromValue >=> (prettyValidate . MiscFeeCurr)
