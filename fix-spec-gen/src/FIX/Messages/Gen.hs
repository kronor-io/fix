{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages.Gen where

import Data.GenValidity
import Data.GenValidity.ByteString ()
import FIX.Components.Gen ()
import FIX.Fields.Gen ()
import FIX.Messages.Envelope
import FIX.Messages.Header
import FIX.Messages.Heartbeat
import FIX.Messages.Logon
import FIX.Messages.Trailer

instance GenValid Header

instance GenValid Trailer

instance (GenValid a) => GenValid (Envelope a)

instance GenValid Heartbeat

instance GenValid Logon
