{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.RefSeqNum where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 45, fieldName = "RefSeqNum", fieldType = FieldTypeSeqNum, fieldValues = []}
newtype RefSeqNum = RefSeqNum {unRefSeqNum :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity RefSeqNum

instance IsField RefSeqNum where
  fieldTag Proxy = 45
  fieldIsData Proxy = False
  fieldToValue = toValue . unRefSeqNum
  fieldFromValue = fromValue >=> (prettyValidate . RefSeqNum)
