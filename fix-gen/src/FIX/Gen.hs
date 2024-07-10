{-# OPTIONS_GHC -Wno-orphans #-}

module FIX.Gen where

import Data.GenValidity
import Data.GenValidity.ByteString ()
import FIX

instance GenValid Message
