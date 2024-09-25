{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Fields.BenchmarkCurveName where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Validity
import FIX.Core
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore" :: String) #-}

-- | FieldSpec
--   { fieldNumber = 221
--   , fieldName = "BenchmarkCurveName"
--   , fieldType = FieldTypeString
--   , fieldValues =
--       [ FieldValueSpec
--           { fieldValueEnum = "MuniAAA" , fieldValueDescription = "MUNIAAA" }
--       , FieldValueSpec
--           { fieldValueEnum = "FutureSWAP"
--           , fieldValueDescription = "FUTURESWAP"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "LIBID" , fieldValueDescription = "LIBID" }
--       , FieldValueSpec
--           { fieldValueEnum = "LIBOR" , fieldValueDescription = "LIBOR" }
--       , FieldValueSpec
--           { fieldValueEnum = "OTHER" , fieldValueDescription = "OTHER" }
--       , FieldValueSpec
--           { fieldValueEnum = "SWAP" , fieldValueDescription = "SWAP" }
--       , FieldValueSpec
--           { fieldValueEnum = "Treasury"
--           , fieldValueDescription = "TREASURY"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "Euribor" , fieldValueDescription = "EURIBOR" }
--       , FieldValueSpec
--           { fieldValueEnum = "Pfandbriefe"
--           , fieldValueDescription = "PFANDBRIEFE"
--           }
--       , FieldValueSpec
--           { fieldValueEnum = "EONIA" , fieldValueDescription = "EONIA" }
--       , FieldValueSpec
--           { fieldValueEnum = "SONIA" , fieldValueDescription = "SONIA" }
--       , FieldValueSpec
--           { fieldValueEnum = "EUREPO" , fieldValueDescription = "EUREPO" }
--       ]
--   }
data BenchmarkCurveName
  = BenchmarkCurveNameMuniaaa
  | BenchmarkCurveNameFutureswap
  | BenchmarkCurveNameLibid
  | BenchmarkCurveNameLibor
  | BenchmarkCurveNameOther
  | BenchmarkCurveNameSwap
  | BenchmarkCurveNameTreasury
  | BenchmarkCurveNameEuribor
  | BenchmarkCurveNamePfandbriefe
  | BenchmarkCurveNameEonia
  | BenchmarkCurveNameSonia
  | BenchmarkCurveNameEurepo
  deriving stock (Show, Eq, Generic)

instance Validity BenchmarkCurveName

instance IsField BenchmarkCurveName where
  fieldTag Proxy = 221
  fieldIsData Proxy = False
  fieldToValue = \case
    BenchmarkCurveNameMuniaaa -> "MuniAAA"
    BenchmarkCurveNameFutureswap -> "FutureSWAP"
    BenchmarkCurveNameLibid -> "LIBID"
    BenchmarkCurveNameLibor -> "LIBOR"
    BenchmarkCurveNameOther -> "OTHER"
    BenchmarkCurveNameSwap -> "SWAP"
    BenchmarkCurveNameTreasury -> "Treasury"
    BenchmarkCurveNameEuribor -> "Euribor"
    BenchmarkCurveNamePfandbriefe -> "Pfandbriefe"
    BenchmarkCurveNameEonia -> "EONIA"
    BenchmarkCurveNameSonia -> "SONIA"
    BenchmarkCurveNameEurepo -> "EUREPO"
  fieldFromValue = \case
    "MuniAAA" -> Right BenchmarkCurveNameMuniaaa
    "FutureSWAP" -> Right BenchmarkCurveNameFutureswap
    "LIBID" -> Right BenchmarkCurveNameLibid
    "LIBOR" -> Right BenchmarkCurveNameLibor
    "OTHER" -> Right BenchmarkCurveNameOther
    "SWAP" -> Right BenchmarkCurveNameSwap
    "Treasury" -> Right BenchmarkCurveNameTreasury
    "Euribor" -> Right BenchmarkCurveNameEuribor
    "Pfandbriefe" -> Right BenchmarkCurveNamePfandbriefe
    "EONIA" -> Right BenchmarkCurveNameEonia
    "SONIA" -> Right BenchmarkCurveNameSonia
    "EUREPO" -> Right BenchmarkCurveNameEurepo
    v -> Left ("Unknown BenchmarkCurveName: " <> show v)