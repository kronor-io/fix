{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.NewOrderMultilegLegsNestedPartiesGroupElem where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.MsgType
import FIX.Fields.NestedPartyID
import FIX.Fields.NestedPartyIDSource
import FIX.Fields.NestedPartyRole
import FIX.Fields.NestedPartyRoleQualifier
import FIX.Fields.NoNestedPartyIDs
import FIX.Groups.Class
import FIX.Groups.NewOrderMultilegLegsNestedPartiesNestedPartySubIDsGroupElem
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "NewOrderMultilegLegsNestedParties"
--   , groupNumberField = "NoNestedPartyIDs"
--   , groupPieces =
--       [ MessagePieceField "NestedPartyID" True
--       , MessagePieceField "NestedPartyIDSource" False
--       , MessagePieceField "NestedPartyRole" False
--       , MessagePieceField "NestedPartyRoleQualifier" False
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "NewOrderMultilegLegsNestedPartiesNestedPartySubIDs"
--             , groupNumberField = "NoNestedPartySubIDs"
--             , groupPieces =
--                 [ MessagePieceField "NestedPartySubID" True
--                 , MessagePieceField "NestedPartySubIDType" False
--                 ]
--             }
--           False
--       ]
--   }
data NewOrderMultilegLegsNestedPartiesGroupElem = NewOrderMultilegLegsNestedPartiesGroupElem
  { newOrderMultilegLegsNestedPartiesGroupElemNestedPartyID :: !NestedPartyID,
    newOrderMultilegLegsNestedPartiesGroupElemNestedPartyIDSource :: !(Maybe NestedPartyIDSource),
    newOrderMultilegLegsNestedPartiesGroupElemNestedPartyRole :: !(Maybe NestedPartyRole),
    newOrderMultilegLegsNestedPartiesGroupElemNestedPartyRoleQualifier :: !(Maybe NestedPartyRoleQualifier),
    newOrderMultilegLegsNestedPartiesGroupElemNewOrderMultilegLegsNestedPartiesNestedPartySubIDsGroup :: ![NewOrderMultilegLegsNestedPartiesNestedPartySubIDsGroupElem]
  }
  deriving stock (Show, Eq, Generic)

instance Validity NewOrderMultilegLegsNestedPartiesGroupElem

instance IsComponent NewOrderMultilegLegsNestedPartiesGroupElem where
  toComponentFields ((NewOrderMultilegLegsNestedPartiesGroupElem {..})) =
    mconcat
      [ requiredFieldB newOrderMultilegLegsNestedPartiesGroupElemNestedPartyID,
        optionalFieldB newOrderMultilegLegsNestedPartiesGroupElemNestedPartyIDSource,
        optionalFieldB newOrderMultilegLegsNestedPartiesGroupElemNestedPartyRole,
        optionalFieldB newOrderMultilegLegsNestedPartiesGroupElemNestedPartyRoleQualifier,
        optionalGroupB newOrderMultilegLegsNestedPartiesGroupElemNewOrderMultilegLegsNestedPartiesNestedPartySubIDsGroup
      ]
  fromComponentFields = do
    newOrderMultilegLegsNestedPartiesGroupElemNestedPartyID <- requiredFieldP
    newOrderMultilegLegsNestedPartiesGroupElemNestedPartyIDSource <- optionalFieldP
    newOrderMultilegLegsNestedPartiesGroupElemNestedPartyRole <- optionalFieldP
    newOrderMultilegLegsNestedPartiesGroupElemNestedPartyRoleQualifier <- optionalFieldP
    newOrderMultilegLegsNestedPartiesGroupElemNewOrderMultilegLegsNestedPartiesNestedPartySubIDsGroup <- optionalGroupP
    pure (NewOrderMultilegLegsNestedPartiesGroupElem {..})

instance IsGroupElement NewOrderMultilegLegsNestedPartiesGroupElem where
  type GroupNumField NewOrderMultilegLegsNestedPartiesGroupElem = NoNestedPartyIDs
  mkGroupNum Proxy = NoNestedPartyIDs
  countGroupNum Proxy = unNoNestedPartyIDs

makeNewOrderMultilegLegsNestedPartiesGroupElem :: NestedPartyID -> NewOrderMultilegLegsNestedPartiesGroupElem
makeNewOrderMultilegLegsNestedPartiesGroupElem newOrderMultilegLegsNestedPartiesGroupElemNestedPartyID =
  let newOrderMultilegLegsNestedPartiesGroupElemNestedPartyIDSource = Nothing
      newOrderMultilegLegsNestedPartiesGroupElemNestedPartyRole = Nothing
      newOrderMultilegLegsNestedPartiesGroupElemNestedPartyRoleQualifier = Nothing
      newOrderMultilegLegsNestedPartiesGroupElemNewOrderMultilegLegsNestedPartiesNestedPartySubIDsGroup = []
   in (NewOrderMultilegLegsNestedPartiesGroupElem {..})
