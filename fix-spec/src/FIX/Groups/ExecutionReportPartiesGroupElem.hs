{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.ExecutionReportPartiesGroupElem where

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
import FIX.Groups.ExecutionReportPartiesPartySubIDsGroupElem
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "ExecutionReportParties"
--   , groupNumberField = "NoPartyIDs"
--   , groupPieces =
--       [ MessagePieceField "PartyID" True
--       , MessagePieceField "PartyIDSource" False
--       , MessagePieceField "PartyRole" False
--       , MessagePieceField "PartyRoleQualifier" False
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "ExecutionReportPartiesPartySubIDs"
--             , groupNumberField = "NoPartySubIDs"
--             , groupPieces =
--                 [ MessagePieceField "PartySubID" True
--                 , MessagePieceField "PartySubIDType" False
--                 ]
--             }
--           False
--       ]
--   }
data ExecutionReportPartiesGroupElem = ExecutionReportPartiesGroupElem
  { executionReportPartiesGroupElemPartyID :: !PartyID,
    executionReportPartiesGroupElemPartyIDSource :: !(Maybe PartyIDSource),
    executionReportPartiesGroupElemPartyRole :: !(Maybe PartyRole),
    executionReportPartiesGroupElemPartyRoleQualifier :: !(Maybe PartyRoleQualifier),
    executionReportPartiesGroupElemExecutionReportPartiesPartySubIDsGroup :: ![ExecutionReportPartiesPartySubIDsGroupElem]
  }
  deriving stock (Show, Eq, Generic)

instance Validity ExecutionReportPartiesGroupElem

instance IsComponent ExecutionReportPartiesGroupElem where
  toComponentFields ((ExecutionReportPartiesGroupElem {..})) =
    mconcat
      [ requiredFieldB executionReportPartiesGroupElemPartyID,
        optionalFieldB executionReportPartiesGroupElemPartyIDSource,
        optionalFieldB executionReportPartiesGroupElemPartyRole,
        optionalFieldB executionReportPartiesGroupElemPartyRoleQualifier,
        optionalGroupB executionReportPartiesGroupElemExecutionReportPartiesPartySubIDsGroup
      ]
  fromComponentFields = do
    executionReportPartiesGroupElemPartyID <- requiredFieldP
    executionReportPartiesGroupElemPartyIDSource <- optionalFieldP
    executionReportPartiesGroupElemPartyRole <- optionalFieldP
    executionReportPartiesGroupElemPartyRoleQualifier <- optionalFieldP
    executionReportPartiesGroupElemExecutionReportPartiesPartySubIDsGroup <- optionalGroupP
    pure (ExecutionReportPartiesGroupElem {..})

instance IsGroupElement ExecutionReportPartiesGroupElem where
  type GroupNumField ExecutionReportPartiesGroupElem = NoPartyIDs
  mkGroupNum Proxy = NoPartyIDs
  countGroupNum Proxy = unNoPartyIDs

makeExecutionReportPartiesGroupElem :: PartyID -> ExecutionReportPartiesGroupElem
makeExecutionReportPartiesGroupElem executionReportPartiesGroupElemPartyID =
  let executionReportPartiesGroupElemPartyIDSource = Nothing
      executionReportPartiesGroupElemPartyRole = Nothing
      executionReportPartiesGroupElemPartyRoleQualifier = Nothing
      executionReportPartiesGroupElemExecutionReportPartiesPartySubIDsGroup = []
   in (ExecutionReportPartiesGroupElem {..})
