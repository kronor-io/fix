{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.QuoteRequestRejectRelatedSymLegsNestedPartiesGroupElem where

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
import FIX.Groups.QuoteRequestRejectRelatedSymLegsNestedPartiesNestedPartySubIDsGroupElem
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "QuoteRequestRejectRelatedSymLegsNestedParties"
--   , groupNumberField = "NoNestedPartyIDs"
--   , groupPieces =
--       [ MessagePieceField "NestedPartyID" True
--       , MessagePieceField "NestedPartyIDSource" False
--       , MessagePieceField "NestedPartyRole" False
--       , MessagePieceField "NestedPartyRoleQualifier" False
--       , MessagePieceGroup
--           GroupSpec
--             { groupName =
--                 "QuoteRequestRejectRelatedSymLegsNestedPartiesNestedPartySubIDs"
--             , groupNumberField = "NoNestedPartySubIDs"
--             , groupPieces =
--                 [ MessagePieceField "NestedPartySubID" True
--                 , MessagePieceField "NestedPartySubIDType" False
--                 ]
--             }
--           False
--       ]
--   }
data QuoteRequestRejectRelatedSymLegsNestedPartiesGroupElem = QuoteRequestRejectRelatedSymLegsNestedPartiesGroupElem
  { quoteRequestRejectRelatedSymLegsNestedPartiesGroupElemNestedPartyID :: !NestedPartyID,
    quoteRequestRejectRelatedSymLegsNestedPartiesGroupElemNestedPartyIDSource :: !(Maybe NestedPartyIDSource),
    quoteRequestRejectRelatedSymLegsNestedPartiesGroupElemNestedPartyRole :: !(Maybe NestedPartyRole),
    quoteRequestRejectRelatedSymLegsNestedPartiesGroupElemNestedPartyRoleQualifier :: !(Maybe NestedPartyRoleQualifier),
    quoteRequestRejectRelatedSymLegsNestedPartiesGroupElemQuoteRequestRejectRelatedSymLegsNestedPartiesNestedPartySubIDsGroup :: ![QuoteRequestRejectRelatedSymLegsNestedPartiesNestedPartySubIDsGroupElem]
  }
  deriving stock (Show, Eq, Generic)

instance Validity QuoteRequestRejectRelatedSymLegsNestedPartiesGroupElem

instance IsComponent QuoteRequestRejectRelatedSymLegsNestedPartiesGroupElem where
  toComponentFields ((QuoteRequestRejectRelatedSymLegsNestedPartiesGroupElem {..})) =
    mconcat
      [ requiredFieldB quoteRequestRejectRelatedSymLegsNestedPartiesGroupElemNestedPartyID,
        optionalFieldB quoteRequestRejectRelatedSymLegsNestedPartiesGroupElemNestedPartyIDSource,
        optionalFieldB quoteRequestRejectRelatedSymLegsNestedPartiesGroupElemNestedPartyRole,
        optionalFieldB quoteRequestRejectRelatedSymLegsNestedPartiesGroupElemNestedPartyRoleQualifier,
        optionalGroupB quoteRequestRejectRelatedSymLegsNestedPartiesGroupElemQuoteRequestRejectRelatedSymLegsNestedPartiesNestedPartySubIDsGroup
      ]
  fromComponentFields = do
    quoteRequestRejectRelatedSymLegsNestedPartiesGroupElemNestedPartyID <- requiredFieldP
    quoteRequestRejectRelatedSymLegsNestedPartiesGroupElemNestedPartyIDSource <- optionalFieldP
    quoteRequestRejectRelatedSymLegsNestedPartiesGroupElemNestedPartyRole <- optionalFieldP
    quoteRequestRejectRelatedSymLegsNestedPartiesGroupElemNestedPartyRoleQualifier <- optionalFieldP
    quoteRequestRejectRelatedSymLegsNestedPartiesGroupElemQuoteRequestRejectRelatedSymLegsNestedPartiesNestedPartySubIDsGroup <- optionalGroupP
    pure (QuoteRequestRejectRelatedSymLegsNestedPartiesGroupElem {..})

instance IsGroupElement QuoteRequestRejectRelatedSymLegsNestedPartiesGroupElem where
  type GroupNumField QuoteRequestRejectRelatedSymLegsNestedPartiesGroupElem = NoNestedPartyIDs
  mkGroupNum Proxy = NoNestedPartyIDs
  countGroupNum Proxy = unNoNestedPartyIDs

makeQuoteRequestRejectRelatedSymLegsNestedPartiesGroupElem :: NestedPartyID -> QuoteRequestRejectRelatedSymLegsNestedPartiesGroupElem
makeQuoteRequestRejectRelatedSymLegsNestedPartiesGroupElem quoteRequestRejectRelatedSymLegsNestedPartiesGroupElemNestedPartyID =
  let quoteRequestRejectRelatedSymLegsNestedPartiesGroupElemNestedPartyIDSource = Nothing
      quoteRequestRejectRelatedSymLegsNestedPartiesGroupElemNestedPartyRole = Nothing
      quoteRequestRejectRelatedSymLegsNestedPartiesGroupElemNestedPartyRoleQualifier = Nothing
      quoteRequestRejectRelatedSymLegsNestedPartiesGroupElemQuoteRequestRejectRelatedSymLegsNestedPartiesNestedPartySubIDsGroup = []
   in (QuoteRequestRejectRelatedSymLegsNestedPartiesGroupElem {..})
