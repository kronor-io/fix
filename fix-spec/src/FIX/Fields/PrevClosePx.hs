{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.PrevClosePx where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 140
--   , fieldName = "PrevClosePx"
--   , fieldType = FieldTypePrice
--   , fieldValues = []
--   }
newtype PrevClosePx = PrevClosePx {unPrevClosePx :: SimpleBytes}
  deriving stock (Show, Eq, Generic)

instance Validity PrevClosePx

instance IsField PrevClosePx where
  fieldTag Proxy = 140
  fieldIsData Proxy = False
  fieldToValue = toValue . unPrevClosePx
  fieldFromValue = fromValue >=> (prettyValidate . PrevClosePx)
