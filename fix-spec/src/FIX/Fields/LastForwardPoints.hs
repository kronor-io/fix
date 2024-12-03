{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LastForwardPoints where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 195
--   , fieldName = "LastForwardPoints"
--   , fieldType = FieldTypePriceOffset
--   , fieldValues = []
--   }
newtype LastForwardPoints = LastForwardPoints {unLastForwardPoints :: PriceOffset}
  deriving stock (Show, Eq, Generic)

instance Validity LastForwardPoints

instance IsField LastForwardPoints where
  fieldTag Proxy = 195
  fieldIsData Proxy = False
  fieldToValue = toValue . unLastForwardPoints
  fieldFromValue = fromValue >=> (prettyValidate . LastForwardPoints)
