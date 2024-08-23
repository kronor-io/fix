{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.CollAsgnID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 902, fieldName = "CollAsgnID", fieldType = FieldTypeString, fieldValues = []}
newtype CollAsgnID = CollAsgnID {unCollAsgnID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity CollAsgnID

instance IsField CollAsgnID where
  fieldTag Proxy = 902
  fieldIsData Proxy = False
  fieldToValue = toValue . unCollAsgnID
  fieldFromValue = fromValue >=> (prettyValidate . CollAsgnID)
