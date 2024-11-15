{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LegCoveredOrUncovered where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 565
--   , fieldName = "LegCoveredOrUncovered"
--   , fieldType = FieldTypeInt
--   , fieldValues = []
--   }
newtype LegCoveredOrUncovered = LegCoveredOrUncovered {unLegCoveredOrUncovered :: Int}
  deriving stock (Show, Eq, Generic)

instance Validity LegCoveredOrUncovered

instance IsField LegCoveredOrUncovered where
  fieldTag Proxy = 565
  fieldIsData Proxy = False
  fieldToValue = toValue . unLegCoveredOrUncovered
  fieldFromValue = fromValue >=> (prettyValidate . LegCoveredOrUncovered)
