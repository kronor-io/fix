{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.MassStatusReqID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 584
--   , fieldName = "MassStatusReqID"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype MassStatusReqID = MassStatusReqID {unMassStatusReqID :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity MassStatusReqID

instance IsField MassStatusReqID where
  fieldTag Proxy = 584
  fieldIsData Proxy = False
  fieldToValue = toValue . unMassStatusReqID
  fieldFromValue = fromValue >=> (prettyValidate . MassStatusReqID)
