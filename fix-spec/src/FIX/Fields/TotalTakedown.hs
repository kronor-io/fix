{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.TotalTakedown where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 237
--   , fieldName = "TotalTakedown"
--   , fieldType = FieldTypeAMT
--   , fieldValues = []
--   }
newtype TotalTakedown = TotalTakedown {unTotalTakedown :: Amount}
  deriving stock (Show, Eq, Generic)

instance Validity TotalTakedown

instance IsField TotalTakedown where
  fieldTag Proxy = 237
  fieldIsData Proxy = False
  fieldToValue = toValue . unTotalTakedown
  fieldFromValue = fromValue >=> (prettyValidate . TotalTakedown)
