{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.MessagesSpec where

import FIX.Core.TestUtils
import FIX.Messages.Gen ()
import FIX.Messages.Heartbeat
import FIX.Messages.Logon
import Test.Syd

spec :: Spec
spec = do messageSpec @Heartbeat; messageSpec @Logon
