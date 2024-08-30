{-# LANGUAGE TypeApplications #-}

module FIX.Messages.EnvelopeSpec where

import FIX.Components.TestUtils
import FIX.Messages.Gen ()
import FIX.Messages.Header
import FIX.Messages.Trailer
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Header" $ do
    genValidSpec @Header
    componentSpec @Header
  describe "Trailer" $ do
    genValidSpec @Trailer
    componentSpec @Trailer
