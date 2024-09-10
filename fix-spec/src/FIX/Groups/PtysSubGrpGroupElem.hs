{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.PtysSubGrpGroupElem where

import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.MsgType
import FIX.Fields.NoPartySubIDs
import FIX.Fields.PartySubID
import FIX.Fields.PartySubIDType
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "PtysSubGrp"
--   , groupNumberField = "NoPartySubIDs"
--   , groupPieces =
--       [ MessagePieceField "PartySubID" True
--       , MessagePieceField "PartySubIDType" False
--       ]
--   }
data PtysSubGrpGroupElem = PtysSubGrpGroupElem
  { ptysSubGrpGroupElemPartySubID :: !PartySubID,
    ptysSubGrpGroupElemPartySubIDType :: !(Maybe PartySubIDType)
  }
  deriving stock (Show, Eq, Generic)

instance Validity PtysSubGrpGroupElem

instance IsComponent PtysSubGrpGroupElem where
  toComponentFields ((PtysSubGrpGroupElem {..})) =
    mconcat
      [ requiredFieldB ptysSubGrpGroupElemPartySubID,
        optionalFieldB ptysSubGrpGroupElemPartySubIDType
      ]
  fromComponentFields = do
    ptysSubGrpGroupElemPartySubID <- requiredFieldP
    ptysSubGrpGroupElemPartySubIDType <- optionalFieldP
    pure (PtysSubGrpGroupElem {..})

instance IsGroupElement PtysSubGrpGroupElem where
  type GroupNumField PtysSubGrpGroupElem = NoPartySubIDs
  mkGroupNum Proxy = NoPartySubIDs
  countGroupNum Proxy = unNoPartySubIDs

makePtysSubGrpGroupElem :: PartySubID -> PtysSubGrpGroupElem
makePtysSubGrpGroupElem ptysSubGrpGroupElemPartySubID =
  let ptysSubGrpGroupElemPartySubIDType = Nothing
   in (PtysSubGrpGroupElem {..})
