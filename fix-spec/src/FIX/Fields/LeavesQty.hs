{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LeavesQty where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 151
--   , fieldName = "LeavesQty"
--   , fieldType = FieldTypeQTY
--   , fieldValues = []
--   }
newtype LeavesQty = LeavesQty {unLeavesQty :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity LeavesQty

instance IsField LeavesQty where
  fieldTag Proxy = 151
  fieldIsData Proxy = False
  fieldToValue = toValue . unLeavesQty
  fieldFromValue = fromValue >=> (prettyValidate . LeavesQty)
