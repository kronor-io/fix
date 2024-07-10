{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.ExecRefID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 19
--   , fieldName = "ExecRefID"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype ExecRefID = ExecRefID {unExecRefID :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity ExecRefID

instance IsField ExecRefID where
  fieldTag Proxy = 19
  fieldIsData Proxy = False
  fieldToValue = toValue . unExecRefID
  fieldFromValue = fromValue >=> (prettyValidate . ExecRefID)
