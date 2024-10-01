{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.QuoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElem where

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
import FIX.Groups.QuoteRequestRelatedSymLegsLegAllocsNestedPartiesNestedPartySubIDsGroupElem
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "QuoteRequestRelatedSymLegsLegAllocsNestedParties"
--   , groupNumberField = "NoNestedPartyIDs"
--   , groupPieces =
--       [ MessagePieceField "NestedPartyID" True
--       , MessagePieceField "NestedPartyIDSource" False
--       , MessagePieceField "NestedPartyRole" False
--       , MessagePieceField "NestedPartyRoleQualifier" False
--       , MessagePieceGroup
--           GroupSpec
--             { groupName =
--                 "QuoteRequestRelatedSymLegsLegAllocsNestedPartiesNestedPartySubIDs"
--             , groupNumberField = "NoNestedPartySubIDs"
--             , groupPieces =
--                 [ MessagePieceField "NestedPartySubID" True
--                 , MessagePieceField "NestedPartySubIDType" False
--                 ]
--             }
--           False
--       ]
--   }
data QuoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElem = QuoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElem
  { quoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElemNestedPartyID :: !NestedPartyID,
    quoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElemNestedPartyIDSource :: !(Maybe NestedPartyIDSource),
    quoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElemNestedPartyRole :: !(Maybe NestedPartyRole),
    quoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElemNestedPartyRoleQualifier :: !(Maybe NestedPartyRoleQualifier),
    quoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElemQuoteRequestRelatedSymLegsLegAllocsNestedPartiesNestedPartySubIDsGroup :: ![QuoteRequestRelatedSymLegsLegAllocsNestedPartiesNestedPartySubIDsGroupElem]
  }
  deriving stock (Show, Eq, Generic)

instance Validity QuoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElem

instance IsComponent QuoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElem where
  toComponentFields ((QuoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElem {..})) =
    mconcat
      [ requiredFieldB quoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElemNestedPartyID,
        optionalFieldB quoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElemNestedPartyIDSource,
        optionalFieldB quoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElemNestedPartyRole,
        optionalFieldB quoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElemNestedPartyRoleQualifier,
        optionalGroupB quoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElemQuoteRequestRelatedSymLegsLegAllocsNestedPartiesNestedPartySubIDsGroup
      ]
  fromComponentFields = do
    quoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElemNestedPartyID <- requiredFieldP
    quoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElemNestedPartyIDSource <- optionalFieldP
    quoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElemNestedPartyRole <- optionalFieldP
    quoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElemNestedPartyRoleQualifier <- optionalFieldP
    quoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElemQuoteRequestRelatedSymLegsLegAllocsNestedPartiesNestedPartySubIDsGroup <- optionalGroupP
    pure (QuoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElem {..})

instance IsGroupElement QuoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElem where
  type GroupNumField QuoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElem = NoNestedPartyIDs
  mkGroupNum Proxy = NoNestedPartyIDs
  countGroupNum Proxy = unNoNestedPartyIDs

makeQuoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElem :: NestedPartyID -> QuoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElem
makeQuoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElem quoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElemNestedPartyID =
  let quoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElemNestedPartyIDSource = Nothing
      quoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElemNestedPartyRole = Nothing
      quoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElemNestedPartyRoleQualifier = Nothing
      quoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElemQuoteRequestRelatedSymLegsLegAllocsNestedPartiesNestedPartySubIDsGroup = []
   in (QuoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElem {..})