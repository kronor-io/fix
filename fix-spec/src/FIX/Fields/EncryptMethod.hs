{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.EncryptMethod where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 98
--   , fieldName = "EncryptMethod"
--   , fieldType = FieldTypeInt
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "0" , fieldValueDescription = "NONE_OTHER" }
--       , FieldValueSpec
--           { fieldValueEnum = "1" , fieldValueDescription = "PKCS" }
--       , FieldValueSpec
--           { fieldValueEnum = "2" , fieldValueDescription = "DES" }
--       , FieldValueSpec
--           { fieldValueEnum = "3" , fieldValueDescription = "PKCS_DES" }
--       , FieldValueSpec
--           { fieldValueEnum = "4" , fieldValueDescription = "PGP_DES" }
--       , FieldValueSpec
--           { fieldValueEnum = "5" , fieldValueDescription = "PGP_DES_MD5" }
--       , FieldValueSpec
--           { fieldValueEnum = "6" , fieldValueDescription = "PEM_DES_MD5" }
--       ]
--   }
data EncryptMethod
  = EncryptMethodNoneOther
  | EncryptMethodPkcs
  | EncryptMethodDes
  | EncryptMethodPkcsDes
  | EncryptMethodPgpDes
  | EncryptMethodPgpDesMd5
  | EncryptMethodPemDesMd5
  deriving stock (Show, Eq, Generic)

instance Validity EncryptMethod

instance IsField EncryptMethod where
  fieldTag Proxy = 98
  fieldIsData Proxy = False
  fieldToValue = \case
    EncryptMethodNoneOther -> "0"
    EncryptMethodPkcs -> "1"
    EncryptMethodDes -> "2"
    EncryptMethodPkcsDes -> "3"
    EncryptMethodPgpDes -> "4"
    EncryptMethodPgpDesMd5 -> "5"
    EncryptMethodPemDesMd5 -> "6"
  fieldFromValue = \case
    "0" -> Right EncryptMethodNoneOther
    "1" -> Right EncryptMethodPkcs
    "2" -> Right EncryptMethodDes
    "3" -> Right EncryptMethodPkcsDes
    "4" -> Right EncryptMethodPgpDes
    "5" -> Right EncryptMethodPgpDesMd5
    "6" -> Right EncryptMethodPemDesMd5
    v -> Left ("Unknown EncryptMethod: " <> show v)
