{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.LastMsgSeqNumProcessed where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 369, fieldName = "LastMsgSeqNumProcessed", fieldType = FieldTypeSeqNum, fieldValues = []}
newtype LastMsgSeqNumProcessed = LastMsgSeqNumProcessed {unLastMsgSeqNumProcessed :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity LastMsgSeqNumProcessed

instance IsField LastMsgSeqNumProcessed where
  fieldTag Proxy = 369
  fieldIsData Proxy = False
  fieldToValue = toValue . unLastMsgSeqNumProcessed
  fieldFromValue = fromValue >=> (prettyValidate . LastMsgSeqNumProcessed)
