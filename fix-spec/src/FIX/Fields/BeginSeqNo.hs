{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.BeginSeqNo where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 7
--   , fieldName = "BeginSeqNo"
--   , fieldType = FieldTypeSeqNum
--   , fieldValues = []
--   }
newtype BeginSeqNo = BeginSeqNo {unBeginSeqNo :: Word}
  deriving stock (Show, Eq, Generic)

instance Validity BeginSeqNo

instance IsField BeginSeqNo where
  fieldTag Proxy = 7
  fieldIsData Proxy = False
  fieldToValue = toValue . unBeginSeqNo
  fieldFromValue = fromValue >=> (prettyValidate . BeginSeqNo)
