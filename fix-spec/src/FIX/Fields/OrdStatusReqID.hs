{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.OrdStatusReqID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 790
--   , fieldName = "OrdStatusReqID"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype OrdStatusReqID = OrdStatusReqID {unOrdStatusReqID :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity OrdStatusReqID

instance IsField OrdStatusReqID where
  fieldTag Proxy = 790
  fieldIsData Proxy = False
  fieldToValue = toValue . unOrdStatusReqID
  fieldFromValue = fromValue >=> (prettyValidate . OrdStatusReqID)
