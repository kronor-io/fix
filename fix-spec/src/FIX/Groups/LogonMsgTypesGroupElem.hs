{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.LogonMsgTypesGroupElem where

import Data.List.NonEmpty (NonEmpty)
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
--   { groupName = "LogonMsgTypes"
--   , groupNumberField = "NoMsgTypes"
--   , groupPieces =
--       [ MessagePieceField "RefMsgType" True
--       , MessagePieceField "MsgDirection" False
--       ]
--   }
data LogonMsgTypesGroupElem = LogonMsgTypesGroupElem
  { logonMsgTypesGroupElemRefMsgType :: !RefMsgType,
    logonMsgTypesGroupElemMsgDirection :: !(Maybe MsgDirection)
  }
  deriving stock (Show, Eq, Generic)

instance Validity LogonMsgTypesGroupElem

instance IsComponent LogonMsgTypesGroupElem where
  toComponentFields ((LogonMsgTypesGroupElem {..})) =
    mconcat
      [ requiredFieldB logonMsgTypesGroupElemRefMsgType,
        optionalFieldB logonMsgTypesGroupElemMsgDirection
      ]
  fromComponentFields = do
    logonMsgTypesGroupElemRefMsgType <- requiredFieldP
    logonMsgTypesGroupElemMsgDirection <- optionalFieldP
    pure (LogonMsgTypesGroupElem {..})

instance IsGroupElement LogonMsgTypesGroupElem where
  type GroupNumField LogonMsgTypesGroupElem = NoMsgTypes
  mkGroupNum Proxy = NoMsgTypes
  countGroupNum Proxy = unNoMsgTypes

makeLogonMsgTypesGroupElem :: RefMsgType -> LogonMsgTypesGroupElem
makeLogonMsgTypesGroupElem logonMsgTypesGroupElemRefMsgType =
  let logonMsgTypesGroupElemMsgDirection = Nothing
   in (LogonMsgTypesGroupElem {..})
