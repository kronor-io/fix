{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.MaturityMonthYear where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 200
--   , fieldName = "MaturityMonthYear"
--   , fieldType = FieldTypeMonthYear
--   , fieldValues = []
--   }
newtype MaturityMonthYear = MaturityMonthYear {unMaturityMonthYear :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity MaturityMonthYear

instance IsField MaturityMonthYear where
  fieldTag Proxy = 200
  fieldIsData Proxy = False
  fieldToValue = toValue . unMaturityMonthYear
  fieldFromValue = fromValue >=> (prettyValidate . MaturityMonthYear)