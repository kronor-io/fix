{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SolicitedFlag where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 377
--   , fieldName = "SolicitedFlag"
--   , fieldType = FieldTypeBoolean
--   , fieldValues = []
--   }
newtype SolicitedFlag = SolicitedFlag {unSolicitedFlag :: Bool}
  deriving stock (Show, Eq, Generic)

instance Validity SolicitedFlag

instance IsField SolicitedFlag where
  fieldTag Proxy = 377
  fieldIsData Proxy = False
  fieldToValue = toValue . unSolicitedFlag
  fieldFromValue = fromValue >=> (prettyValidate . SolicitedFlag)