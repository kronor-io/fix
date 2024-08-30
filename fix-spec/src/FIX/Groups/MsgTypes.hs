{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.MsgTypes where

import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.MsgDirection
import FIX.Fields.MsgType
import FIX.Fields.NoMsgTypes
import FIX.Fields.RefMsgType
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "NoMsgTypes"
--   , groupPieces =
--       [ MessagePieceField "RefMsgType" True
--       , MessagePieceField "MsgDirection" False
--       ]
--   }
data MsgTypes = MsgTypes
  { msgTypesRefMsgType :: !RefMsgType,
    msgTypesMsgDirection :: !(Maybe MsgDirection)
  }
  deriving stock (Show, Eq, Generic)

instance Validity MsgTypes

instance IsComponent MsgTypes where
  toComponentFields ((MsgTypes {..})) =
    concat
      [ requiredFieldB msgTypesRefMsgType,
        optionalFieldB msgTypesMsgDirection
      ]
  fromComponentFields = do
    msgTypesRefMsgType <- requiredFieldP
    msgTypesMsgDirection <- optionalFieldP
    pure (MsgTypes {..})

instance IsGroupElement MsgTypes where
  type GroupNumField MsgTypes = NoMsgTypes
  mkGroupNum Proxy = NoMsgTypes
  countGroupNum Proxy = unNoMsgTypes