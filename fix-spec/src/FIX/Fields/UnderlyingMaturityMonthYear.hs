{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.UnderlyingMaturityMonthYear where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 313, fieldName = "UnderlyingMaturityMonthYear", fieldType = FieldTypeMonthYear, fieldValues = []}
newtype UnderlyingMaturityMonthYear = UnderlyingMaturityMonthYear {unUnderlyingMaturityMonthYear :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity UnderlyingMaturityMonthYear

instance IsField UnderlyingMaturityMonthYear where
  fieldTag Proxy = 313
  fieldIsData Proxy = False
  fieldToValue = toValue . unUnderlyingMaturityMonthYear
  fieldFromValue = fromValue >=> (prettyValidate . UnderlyingMaturityMonthYear)
