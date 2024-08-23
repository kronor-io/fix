{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.ContraLegRefID where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core (IsField (..), IsFieldType (..))
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- FieldSpec {fieldNumber = 655, fieldName = "ContraLegRefID", fieldType = FieldTypeString, fieldValues = []}
newtype ContraLegRefID = ContraLegRefID {unContraLegRefID :: ByteString}
  deriving stock (Show, Eq, Generic)

instance Validity ContraLegRefID

instance IsField ContraLegRefID where
  fieldTag Proxy = 655
  fieldIsData Proxy = False
  fieldToValue = toValue . unContraLegRefID
  fieldFromValue = fromValue >=> (prettyValidate . ContraLegRefID)
