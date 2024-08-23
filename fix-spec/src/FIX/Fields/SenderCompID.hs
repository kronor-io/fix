{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SenderCompID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 49, fieldName = "SenderCompID", fieldType = FieldTypeString, fieldValues = []}
newtype SenderCompID = SenderCompID {unSenderCompID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity SenderCompID

instance IsField SenderCompID where
  fieldTag Proxy = 49
  fieldIsData Proxy = False
  fieldToValue = toValue . unSenderCompID
  fieldFromValue = fromValue >=> (prettyValidate . SenderCompID)
