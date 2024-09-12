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
import FIX.Messages.NewOrderSingle
import FIX.Messages.News
import FIX.Messages.Quote
import FIX.Messages.QuoteCancel
import FIX.Messages.QuoteRequest
import FIX.Messages.QuoteRequestReject
import FIX.Messages.Reject
import FIX.Messages.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe
    "Logon"
    ( do
        genValidSpec @Logon
        componentSpec @Logon
        messageSpec @Logon "Logon"
    )
  describe
    "Heartbeat"
    ( do
        genValidSpec @Heartbeat
        componentSpec @Heartbeat
        messageSpec @Heartbeat "Heartbeat"
    )
  describe
    "Reject"
    ( do
        genValidSpec @Reject
        componentSpec @Reject
        messageSpec @Reject "Reject"
    )
  describe
    "Logout"
    ( do
        genValidSpec @Logout
        componentSpec @Logout
        messageSpec @Logout "Logout"
    )
  describe
    "News"
    ( do
        genValidSpec @News
        componentSpec @News
        messageSpec @News "News"
    )
  describe
    "QuoteRequest"
    ( do
        genValidSpec @QuoteRequest
        componentSpec @QuoteRequest
        messageSpec @QuoteRequest "QuoteRequest"
    )
  describe
    "QuoteRequestReject"
    ( do
        genValidSpec @QuoteRequestReject
        componentSpec @QuoteRequestReject
        messageSpec @QuoteRequestReject "QuoteRequestReject"
    )
  describe
    "Quote"
    ( do
        genValidSpec @Quote
        componentSpec @Quote
        messageSpec @Quote "Quote"
    )
  describe
    "QuoteCancel"
    ( do
        genValidSpec @QuoteCancel
        componentSpec @QuoteCancel
        messageSpec @QuoteCancel "QuoteCancel"
    )
  describe
    "NewOrderSingle"
    ( do
        genValidSpec @NewOrderSingle
        componentSpec @NewOrderSingle
        messageSpec @NewOrderSingle "NewOrderSingle"
    )
