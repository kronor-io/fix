{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.NumTickets where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 395, fieldName = "NumTickets", fieldType = FieldTypeInt, fieldValues = []}
newtype NumTickets = NumTickets {unNumTickets :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity NumTickets

instance IsField NumTickets where
  fieldTag Proxy = 395
  fieldIsData Proxy = False
  fieldToValue = toValue . unNumTickets
  fieldFromValue = fromValue >=> (prettyValidate . NumTickets)
