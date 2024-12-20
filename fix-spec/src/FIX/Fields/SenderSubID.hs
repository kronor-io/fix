{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SenderSubID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 50
--   , fieldName = "SenderSubID"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype SenderSubID = SenderSubID {unSenderSubID :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity SenderSubID

instance IsField SenderSubID where
  fieldTag Proxy = 50
  fieldIsData Proxy = False
  fieldToValue = toValue . unSenderSubID
  fieldFromValue = fromValue >=> (prettyValidate . SenderSubID)
