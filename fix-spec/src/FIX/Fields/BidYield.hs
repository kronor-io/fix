{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.BidYield where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 632
--   , fieldName = "BidYield"
--   , fieldType = FieldTypePercentage
--   , fieldValues = []
--   }
newtype BidYield = BidYield {unBidYield :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity BidYield

instance IsField BidYield where
  fieldTag Proxy = 632
  fieldIsData Proxy = False
  fieldToValue = toValue . unBidYield
  fieldFromValue = fromValue >=> (prettyValidate . BidYield)
