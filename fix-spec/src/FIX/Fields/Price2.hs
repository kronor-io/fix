{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.Price2 where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 640
--   , fieldName = "Price2"
--   , fieldType = FieldTypePrice
--   , fieldValues = []
--   }
newtype Price2 = Price2 {unPrice2 :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity Price2

instance IsField Price2 where
  fieldTag Proxy = 640
  fieldIsData Proxy = False
  fieldToValue = toValue . unPrice2
  fieldFromValue = fromValue >=> (prettyValidate . Price2)