{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.SplitSettlDate2 where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 14102
--   , fieldName = "SplitSettlDate2"
--   , fieldType = FieldTypeLocalMktDate
--   , fieldValues = []
--   }
newtype SplitSettlDate2 = SplitSettlDate2 {unSplitSettlDate2 :: LocalMktDate}
  deriving stock (Show, Eq, Generic)

instance Validity SplitSettlDate2

instance IsField SplitSettlDate2 where
  fieldTag Proxy = 14102
  fieldIsData Proxy = False
  fieldToValue = toValue . unSplitSettlDate2
  fieldFromValue = fromValue >=> (prettyValidate . SplitSettlDate2)
