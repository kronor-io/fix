{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.ClOrdLinkID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 583
--   , fieldName = "ClOrdLinkID"
--   , fieldType = FieldTypeString
--   , fieldValues = []
--   }
newtype ClOrdLinkID = ClOrdLinkID {unClOrdLinkID :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity ClOrdLinkID

instance IsField ClOrdLinkID where
  fieldTag Proxy = 583
  fieldIsData Proxy = False
  fieldToValue = toValue . unClOrdLinkID
  fieldFromValue = fromValue >=> (prettyValidate . ClOrdLinkID)
