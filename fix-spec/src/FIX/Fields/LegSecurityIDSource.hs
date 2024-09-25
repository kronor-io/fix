{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegSecurityIDSource where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 603
--   , fieldName = "LegSecurityIDSource"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype LegSecurityIDSource = LegSecurityIDSource {unLegSecurityIDSource :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity LegSecurityIDSource

instance IsField LegSecurityIDSource where
  fieldTag Proxy = 603
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegSecurityIDSource
  fieldFromValue = fromValue >=> (prettyValidate . LegSecurityIDSource)
