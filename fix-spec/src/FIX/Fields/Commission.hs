{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.Commission where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 12
--   , fieldName = "Commission"
--   , fieldType = FieldTypeAMT
--   , fieldValues = []
--   }
newtype Commission = Commission {unCommission :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity Commission

instance IsField Commission where
  fieldTag Proxy = 12
  fieldIsData Proxy = False
  fieldToValue = toValue . unCommission
  fieldFromValue = fromValue >=> (prettyValidate . Commission)
