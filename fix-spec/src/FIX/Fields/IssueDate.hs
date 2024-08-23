{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.IssueDate where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 225, fieldName = "IssueDate", fieldType = FieldTypeLocalMktDate, fieldValues = []}
newtype IssueDate = IssueDate {unIssueDate :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity IssueDate

instance IsField IssueDate where
  fieldTag Proxy = 225
  fieldIsData Proxy = False
  fieldToValue = toValue . unIssueDate
  fieldFromValue = fromValue >=> (prettyValidate . IssueDate)
