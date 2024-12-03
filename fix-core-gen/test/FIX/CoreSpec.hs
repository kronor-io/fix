{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FIX.CoreSpec (spec) where

import Data.ByteString (ByteString)
import Data.Time
import FIX.Core
import FIX.Core.Gen ()
import FIX.Core.TestUtils
import Test.Syd

spec :: Spec
spec = do
  describe "ByteString" $ do
    fieldTypeSpec @ByteString
  describe "Int" $ do
    fieldTypeSpec @Int
  describe "Word" $ do
    fieldTypeSpec @Word
  describe "Qty" $ do
    fieldTypeSpec @Qty
    fieldTypeExampleSpec
      "0"
      (Qty 0)
    fieldTypeExampleSpec
      "0.5"
      (Qty 0.5)
  describe "Amount" $ do
    fieldTypeSpec @Amount
    fieldTypeExampleSpec
      "0"
      (Amount 0)
    fieldTypeExampleSpec
      "0.5"
      (Amount 0.5)
  describe "PriceVal" $ do
    fieldTypeSpec @PriceVal
    fieldTypeExampleSpec
      "0"
      (PriceVal 0)
    fieldTypeExampleSpec
      "0.5"
      (PriceVal 0.5)
  describe "PriceOffset" $ do
    fieldTypeSpec @PriceOffset
    fieldTypeExampleSpec
      "0"
      (PriceOffset 0)
    fieldTypeExampleSpec
      "0.5"
      (PriceOffset 0.5)
  describe "LocalMktDate" $ do
    fieldTypeExampleSpec
      "20190605"
      (LocalMktDate (fromGregorian 2019 06 05))
    fieldTypeExampleSpec
      "20190607"
      (LocalMktDate (fromGregorian 2019 06 07))
    fieldTypeSpec @LocalMktDate
  describe "UTCTimestamp" $ do
    fieldTypeExampleSpec
      "20190605-11:57:29.363"
      (UTCTimestamp (UTCTime (fromGregorian 2019 06 05) (timeOfDayToTime (TimeOfDay 11 57 29.363))))
    fieldTypeExampleSpec
      "20190605-11:57:29.360"
      (UTCTimestamp (UTCTime (fromGregorian 2019 06 05) (timeOfDayToTime (TimeOfDay 11 57 29.360))))
    fieldTypeSpec @UTCTimestamp
