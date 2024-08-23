{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.DeliverToLocationID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 145, fieldName = "DeliverToLocationID", fieldType = FieldTypeString, fieldValues = []}
newtype DeliverToLocationID = DeliverToLocationID {unDeliverToLocationID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity DeliverToLocationID

instance IsField DeliverToLocationID where
  fieldTag Proxy = 145
  fieldIsData Proxy = False
  fieldToValue = toValue . unDeliverToLocationID
  fieldFromValue = fromValue >=> (prettyValidate . DeliverToLocationID)
