{-# LANGUAGE ScopedTypeVariables #-}

module FIX.Messages.Class where

import Data.Proxy
import FIX.Components.Class
import FIX.Fields.MsgType

class (IsComponent a) => IsMessage a where
  messageType :: Proxy a -> MsgType
