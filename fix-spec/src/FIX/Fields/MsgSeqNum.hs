{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.MsgSeqNum where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 34, fieldName = "MsgSeqNum", fieldType = FieldTypeSeqNum, fieldValues = []}
newtype MsgSeqNum = MsgSeqNum {unMsgSeqNum :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity MsgSeqNum

instance IsField MsgSeqNum where
  fieldTag Proxy = 34
  fieldIsData Proxy = False
  fieldToValue = toValue . unMsgSeqNum
  fieldFromValue = fromValue >=> (prettyValidate . MsgSeqNum)
