{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.PartyIDsGroupElem where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.MsgType
import FIX.Fields.NoPartyIDs
import FIX.Fields.PartyID
import FIX.Fields.PartyIDSource
import FIX.Fields.PartyRole
import FIX.Fields.PartyRoleQualifier
import FIX.Groups.Class
import FIX.Groups.PartySubIDsGroupElem
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "NoPartyIDs"
--   , groupNumberField = "NoPartyIDs"
--   , groupPieces =
--       [ MessagePieceField "PartyID" True
--       , MessagePieceField "PartyIDSource" False
--       , MessagePieceField "PartyRole" False
--       , MessagePieceField "PartyRoleQualifier" False
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "NoPartySubIDs"
--             , groupNumberField = "NoPartySubIDs"
--             , groupPieces =
--                 [ MessagePieceField "PartySubID" True
--                 , MessagePieceField "PartySubIDType" False
--                 ]
--             }
--           False
--       ]
--   }
data PartyIDsGroupElem = PartyIDsGroupElem
  { partyIDsGroupElemPartyID :: !PartyID,
    partyIDsGroupElemPartyIDSource :: !(Maybe PartyIDSource),
    partyIDsGroupElemPartyRole :: !(Maybe PartyRole),
    partyIDsGroupElemPartyRoleQualifier :: !(Maybe PartyRoleQualifier),
    partyIDsGroupElemPartySubIDsGroup :: ![PartySubIDsGroupElem]
  }
  deriving stock (Show, Eq, Generic)

instance Validity PartyIDsGroupElem

instance IsComponent PartyIDsGroupElem where
  toComponentFields ((PartyIDsGroupElem {..})) =
    mconcat
      [ requiredFieldB partyIDsGroupElemPartyID,
        optionalFieldB partyIDsGroupElemPartyIDSource,
        optionalFieldB partyIDsGroupElemPartyRole,
        optionalFieldB partyIDsGroupElemPartyRoleQualifier,
        optionalGroupB partyIDsGroupElemPartySubIDsGroup
      ]
  fromComponentFields = do
    partyIDsGroupElemPartyID <- requiredFieldP
    partyIDsGroupElemPartyIDSource <- optionalFieldP
    partyIDsGroupElemPartyRole <- optionalFieldP
    partyIDsGroupElemPartyRoleQualifier <- optionalFieldP
    partyIDsGroupElemPartySubIDsGroup <- optionalGroupP
    pure (PartyIDsGroupElem {..})

instance IsGroupElement PartyIDsGroupElem where
  type GroupNumField PartyIDsGroupElem = NoPartyIDs
  mkGroupNum Proxy = NoPartyIDs
  countGroupNum Proxy = unNoPartyIDs

makePartyIDsGroupElem :: PartyID -> PartyIDsGroupElem
makePartyIDsGroupElem partyIDsGroupElemPartyID =
  let partyIDsGroupElemPartyIDSource = Nothing
      partyIDsGroupElemPartyRole = Nothing
      partyIDsGroupElemPartyRoleQualifier = Nothing
      partyIDsGroupElemPartySubIDsGroup = []
   in (PartyIDsGroupElem {..})
