{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.MessagesSpec where

import FIX.Components.TestUtils
import FIX.Messages.Gen ()
import FIX.Messages.Heartbeat
import FIX.Messages.Logon
import FIX.Messages.Logout
import FIX.Messages.Quote
import FIX.Messages.QuoteRequest
import FIX.Messages.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe
    "Heartbeat"
    ( do
        genValidSpec @Heartbeat
        componentSpec @Heartbeat
        messageSpec @Heartbeat "Heartbeat"
    )
  describe
    "Logout"
    ( do
        genValidSpec @Logout
        componentSpec @Logout
        messageSpec @Logout "Logout"
    )
  describe
    "Logon"
    ( do
        genValidSpec @Logon
        componentSpec @Logon
        messageSpec @Logon "Logon"
    )
  describe
    "QuoteRequest"
    ( do
        genValidSpec @QuoteRequest
        componentSpec @QuoteRequest
        messageSpec @QuoteRequest "QuoteRequest"
    )
  describe
    "Quote"
    ( do
        genValidSpec @Quote
        componentSpec @Quote
        messageSpec @Quote "Quote"
    )
