{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.DeliverToSubID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec {fieldNumber = 129, fieldName = "DeliverToSubID", fieldType = FieldTypeString, fieldValues = []}
newtype DeliverToSubID = DeliverToSubID {unDeliverToSubID :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity DeliverToSubID

instance IsField DeliverToSubID where
  fieldTag Proxy = 129
  fieldIsData Proxy = False
  fieldToValue = toValue . unDeliverToSubID
  fieldFromValue = fromValue >=> (prettyValidate . DeliverToSubID)
