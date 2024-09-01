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
import FIX.Components.NstdPtysSubGrp
import FIX.Fields.MsgType
import FIX.Fields.NestedPartyID
import FIX.Fields.NestedPartyIDSource
import FIX.Fields.NestedPartyRole
import FIX.Fields.NoNestedPartyIDs
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "NoNestedPartyIDs"
--   , groupPieces =
--       [ MessagePieceField "NestedPartyID" True
--       , MessagePieceField "NestedPartyIDSource" False
--       , MessagePieceField "NestedPartyRole" False
--       , MessagePieceComponent "NstdPtysSubGrp" True
--       ]
--   }
data NestedPartyIDsGroupElem = NestedPartyIDsGroupElem
  { nestedPartyIDsGroupElemNestedPartyID :: !NestedPartyID,
    nestedPartyIDsGroupElemNestedPartyIDSource :: !(Maybe NestedPartyIDSource),
    nestedPartyIDsGroupElemNestedPartyRole :: !(Maybe NestedPartyRole),
    nestedPartyIDsGroupElemNstdPtysSubGrp :: !NstdPtysSubGrp
  }
  deriving stock (Show, Eq, Generic)

instance Validity NestedPartyIDsGroupElem

instance IsComponent NestedPartyIDsGroupElem where
  toComponentFields ((NestedPartyIDsGroupElem {..})) =
    mconcat
      [ requiredFieldB nestedPartyIDsGroupElemNestedPartyID,
        optionalFieldB nestedPartyIDsGroupElemNestedPartyIDSource,
        optionalFieldB nestedPartyIDsGroupElemNestedPartyRole,
        requiredComponentB nestedPartyIDsGroupElemNstdPtysSubGrp
      ]
  fromComponentFields = do
    nestedPartyIDsGroupElemNestedPartyID <- requiredFieldP
    nestedPartyIDsGroupElemNestedPartyIDSource <- optionalFieldP
    nestedPartyIDsGroupElemNestedPartyRole <- optionalFieldP
    nestedPartyIDsGroupElemNstdPtysSubGrp <- requiredComponentP
    pure (NestedPartyIDsGroupElem {..})

instance IsGroupElement NestedPartyIDsGroupElem where
  type GroupNumField NestedPartyIDsGroupElem = NoNestedPartyIDs
  mkGroupNum Proxy = NoNestedPartyIDs
  countGroupNum Proxy = unNoNestedPartyIDs
