{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegSecurityAltIDSource where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 606
--   , fieldName = "LegSecurityAltIDSource"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype LegSecurityAltIDSource = LegSecurityAltIDSource {unLegSecurityAltIDSource :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity LegSecurityAltIDSource

instance IsField LegSecurityAltIDSource where
  fieldTag Proxy = 606
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegSecurityAltIDSource
  fieldFromValue = fromValue >=> (prettyValidate . LegSecurityAltIDSource)
