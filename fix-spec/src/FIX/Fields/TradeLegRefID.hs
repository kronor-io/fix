{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.TradeLegRefID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore" #-}

-- FieldSpec {fieldNumber = 824, fieldName = "TradeLegRefID", fieldType = FieldTypeString, fieldValues = []}
newtype TradeLegRefID = TradeLegRefID {unTradeLegRefID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity TradeLegRefID

instance IsField TradeLegRefID where
  fieldTag Proxy = 824
  fieldIsData Proxy = False
  fieldToValue = toValue . unTradeLegRefID
  fieldFromValue = fromValue >=> (prettyValidate . TradeLegRefID)
