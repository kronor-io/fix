{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.NextExpectedMsgSeqNum where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec {fieldNumber = 789, fieldName = "NextExpectedMsgSeqNum", fieldType = FieldTypeSeqNum, fieldValues = []}
newtype NextExpectedMsgSeqNum = NextExpectedMsgSeqNum {unNextExpectedMsgSeqNum :: Word}
  deriving stock (Show, Eq, Generic)

instance Validity NextExpectedMsgSeqNum

instance IsField NextExpectedMsgSeqNum where
  fieldTag Proxy = 789
  fieldIsData Proxy = False
  fieldToValue = toValue . unNextExpectedMsgSeqNum
  fieldFromValue = fromValue >=> (prettyValidate . NextExpectedMsgSeqNum)
