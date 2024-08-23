{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.UnderlyingLastQty where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 652, fieldName = "UnderlyingLastQty", fieldType = FieldTypeQTY, fieldValues = []}
newtype UnderlyingLastQty = UnderlyingLastQty {unUnderlyingLastQty :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity UnderlyingLastQty

instance IsField UnderlyingLastQty where
  fieldTag Proxy = 652
  fieldIsData Proxy = False
  fieldToValue = toValue . unUnderlyingLastQty
  fieldFromValue = fromValue >=> (prettyValidate . UnderlyingLastQty)
