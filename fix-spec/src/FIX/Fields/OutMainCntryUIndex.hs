{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.OutMainCntryUIndex where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 412, fieldName = "OutMainCntryUIndex", fieldType = FieldTypeAMT, fieldValues = []}
newtype OutMainCntryUIndex = OutMainCntryUIndex {unOutMainCntryUIndex :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity OutMainCntryUIndex

instance IsField OutMainCntryUIndex where
  fieldTag Proxy = 412
  fieldIsData Proxy = False
  fieldToValue = toValue . unOutMainCntryUIndex
  fieldFromValue = fromValue >=> (prettyValidate . OutMainCntryUIndex)
