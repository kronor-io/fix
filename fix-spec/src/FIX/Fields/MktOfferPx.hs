{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.MktOfferPx where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 646, fieldName = "MktOfferPx", fieldType = FieldTypePrice, fieldValues = []}
newtype MktOfferPx = MktOfferPx {unMktOfferPx :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity MktOfferPx

instance IsField MktOfferPx where
  fieldTag Proxy = 646
  fieldIsData Proxy = False
  fieldToValue = toValue . unMktOfferPx
  fieldFromValue = fromValue >=> (prettyValidate . MktOfferPx)
