{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.NestedPartyIDsGroupElem where

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
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "NoNestedPartyIDs"
--   , groupNumberField = "NoNestedPartyIDs"
--   , groupPieces =
--       [ MessagePieceField "NestedPartyID" True
--       , MessagePieceField "NestedPartyIDSource" False
--       , MessagePieceField "NestedPartyRole" False
--       , MessagePieceField "NestedPartyRoleQualifier" False
--       ]
--   }
data NestedPartyIDsGroupElem = NestedPartyIDsGroupElem
  { nestedPartyIDsGroupElemNestedPartyID :: !NestedPartyID,
    nestedPartyIDsGroupElemNestedPartyIDSource :: !(Maybe NestedPartyIDSource),
    nestedPartyIDsGroupElemNestedPartyRole :: !(Maybe NestedPartyRole),
    nestedPartyIDsGroupElemNestedPartyRoleQualifier :: !(Maybe NestedPartyRoleQualifier)
  }
  deriving stock (Show, Eq, Generic)

instance Validity NestedPartyIDsGroupElem

instance IsComponent NestedPartyIDsGroupElem where
  toComponentFields ((NestedPartyIDsGroupElem {..})) =
    mconcat
      [ requiredFieldB nestedPartyIDsGroupElemNestedPartyID,
        optionalFieldB nestedPartyIDsGroupElemNestedPartyIDSource,
        optionalFieldB nestedPartyIDsGroupElemNestedPartyRole,
        optionalFieldB nestedPartyIDsGroupElemNestedPartyRoleQualifier
      ]
  fromComponentFields = do
    nestedPartyIDsGroupElemNestedPartyID <- requiredFieldP
    nestedPartyIDsGroupElemNestedPartyIDSource <- optionalFieldP
    nestedPartyIDsGroupElemNestedPartyRole <- optionalFieldP
    nestedPartyIDsGroupElemNestedPartyRoleQualifier <- optionalFieldP
    pure (NestedPartyIDsGroupElem {..})

instance IsGroupElement NestedPartyIDsGroupElem where
  type GroupNumField NestedPartyIDsGroupElem = NoNestedPartyIDs
  mkGroupNum Proxy = NoNestedPartyIDs
  countGroupNum Proxy = unNoNestedPartyIDs

makeNestedPartyIDsGroupElem :: NestedPartyID -> NestedPartyIDsGroupElem
makeNestedPartyIDsGroupElem nestedPartyIDsGroupElemNestedPartyID =
  let nestedPartyIDsGroupElemNestedPartyIDSource = Nothing
      nestedPartyIDsGroupElemNestedPartyRole = Nothing
      nestedPartyIDsGroupElemNestedPartyRoleQualifier = Nothing
   in (NestedPartyIDsGroupElem {..})