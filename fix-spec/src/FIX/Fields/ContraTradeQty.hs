{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.ContraTradeQty where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 437, fieldName = "ContraTradeQty", fieldType = FieldTypeQTY, fieldValues = []}
newtype ContraTradeQty = ContraTradeQty {unContraTradeQty :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity ContraTradeQty

instance IsField ContraTradeQty where
  fieldTag Proxy = 437
  fieldIsData Proxy = False
  fieldToValue = toValue . unContraTradeQty
  fieldFromValue = fromValue >=> (prettyValidate . ContraTradeQty)
