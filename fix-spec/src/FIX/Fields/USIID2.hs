{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.USIID2 where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 7608
--   , fieldName = "USIID2"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype USIID2 = USIID2 {unUSIID2 :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity USIID2

instance IsField USIID2 where
  fieldTag Proxy = 7608
  fieldIsData Proxy = False
  fieldToValue = toValue . unUSIID2
  fieldFromValue = fromValue >=> (prettyValidate . USIID2)