{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages.QuoteRequest where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.DayCount
import FIX.Fields.ExecutionVenueType
import FIX.Fields.MsgType
import FIX.Fields.OptionDate
import FIX.Fields.ProductType
import FIX.Fields.QuoteReqID
import FIX.Fields.RefSpotDate
import FIX.Groups.Class
import FIX.Groups.CustomFieldsGroupElem
import FIX.Groups.OrderAttributeGroupElem
import FIX.Groups.PartyIDsGroupElem
import FIX.Groups.QuotReqGrpGroupElem
import FIX.Groups.RegulatoryTradeIDGroupElem
import FIX.Messages.Class
import GHC.Generics (Generic)

-- | MessageSpec
--   { messageName = "QuoteRequest"
--   , messageType = "R"
--   , messageCategory = "app"
--   , messagePieces =
--       [ MessagePieceField "QuoteReqID" True
--       , MessagePieceField "RefSpotDate" False
--       , MessagePieceField "ProductType" True
--       , MessagePieceField "ExecutionVenueType" False
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "NoCustomFields"
--             , groupNumberField = "NoCustomFields"
--             , groupPieces =
--                 [ MessagePieceField "CustomFieldName" True
--                 , MessagePieceField "CustomFieldValue" True
--                 ]
--             }
--           False
--       , MessagePieceField "DayCount" False
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "QuotReqGrp"
--             , groupNumberField = "NoRelatedSym"
--             , groupPieces =
--                 [ MessagePieceField "Symbol" True
--                 , MessagePieceField "MaturityDate" False
--                 , MessagePieceField "MaturityDate2" False
--                 , MessagePieceField "Issuer" False
--                 , MessagePieceField "QuoteType" True
--                 , MessagePieceField "Side" False
--                 , MessagePieceField "OrderQty" True
--                 , MessagePieceField "SettlDate" True
--                 , MessagePieceField "SettlDate2" False
--                 , MessagePieceField "OrderQty2" False
--                 , MessagePieceField "Currency" False
--                 , MessagePieceField "Account" True
--                 , MessagePieceField "ExpireTime" False
--                 , MessagePieceGroup
--                     GroupSpec
--                       { groupName = "NoAllocs"
--                       , groupNumberField = "NoAllocs"
--                       , groupPieces =
--                           [ MessagePieceField "AllocAccount" True
--                           , MessagePieceField "AllocQty" True
--                           , MessagePieceGroup
--                               GroupSpec
--                                 { groupName = "NoNestedPartyIDs"
--                                 , groupNumberField = "NoNestedPartyIDs"
--                                 , groupPieces =
--                                     [ MessagePieceField "NestedPartyID" True
--                                     , MessagePieceField "NestedPartyIDSource" False
--                                     , MessagePieceField "NestedPartyRole" False
--                                     , MessagePieceField "NestedPartyRoleQualifier" False
--                                     ]
--                                 }
--                               False
--                           ]
--                       }
--                     False
--                 , MessagePieceGroup
--                     GroupSpec
--                       { groupName = "QuotReqLegsGrp"
--                       , groupNumberField = "NoLegs"
--                       , groupPieces =
--                           [ MessagePieceField "LegSymbol" True
--                           , MessagePieceField "LegMaturityDate" True
--                           , MessagePieceField "LegSide" True
--                           , MessagePieceField "LegQty" True
--                           , MessagePieceGroup
--                               GroupSpec
--                                 { groupName = "NoLegAllocs"
--                                 , groupNumberField = "NoLegAllocs"
--                                 , groupPieces =
--                                     [ MessagePieceField "LegAllocAccount" True
--                                     , MessagePieceField "LegAllocQty" True
--                                     , MessagePieceGroup
--                                         GroupSpec
--                                           { groupName = "NoNestedPartyIDs"
--                                           , groupNumberField = "NoNestedPartyIDs"
--                                           , groupPieces =
--                                               [ MessagePieceField "NestedPartyID" True
--                                               , MessagePieceField "NestedPartyIDSource" False
--                                               , MessagePieceField "NestedPartyRole" False
--                                               , MessagePieceField "NestedPartyRoleQualifier" False
--                                               ]
--                                           }
--                                         False
--                                     ]
--                                 }
--                               True
--                           , MessagePieceField "LegRefID" True
--                           , MessagePieceField "LegSettlDate" True
--                           ]
--                       }
--                     False
--                 ]
--             }
--           True
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "NoPartyIDs"
--             , groupNumberField = "NoPartyIDs"
--             , groupPieces =
--                 [ MessagePieceField "PartyID" True
--                 , MessagePieceField "PartyIDSource" False
--                 , MessagePieceField "PartyRole" False
--                 , MessagePieceField "PartyRoleQualifier" False
--                 ]
--             }
--           False
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "NoRegulatoryTradeID"
--             , groupNumberField = "NoRegulatoryTradeID"
--             , groupPieces =
--                 [ MessagePieceField "RegulatoryTradeID" True
--                 , MessagePieceField "RegulatoryTradeIDType" False
--                 ]
--             }
--           False
--       , MessagePieceField "OptionDate" False
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "NoOrderAttribute"
--             , groupNumberField = "NoOrderAttribute"
--             , groupPieces =
--                 [ MessagePieceField "OrderAttributeType" True
--                 , MessagePieceField "OrderAttributeValue" False
--                 ]
--             }
--           False
--       ]
--   }
data QuoteRequest = QuoteRequest
  { quoteRequestQuoteReqID :: !QuoteReqID,
    quoteRequestRefSpotDate :: !(Maybe RefSpotDate),
    quoteRequestProductType :: !ProductType,
    quoteRequestExecutionVenueType :: !(Maybe ExecutionVenueType),
    quoteRequestCustomFieldsGroup :: ![CustomFieldsGroupElem],
    quoteRequestDayCount :: !(Maybe DayCount),
    quoteRequestQuotReqGrpGroup :: !(NonEmpty QuotReqGrpGroupElem),
    quoteRequestPartyIDsGroup :: ![PartyIDsGroupElem],
    quoteRequestRegulatoryTradeIDGroup :: ![RegulatoryTradeIDGroupElem],
    quoteRequestOptionDate :: !(Maybe OptionDate),
    quoteRequestOrderAttributeGroup :: ![OrderAttributeGroupElem]
  }
  deriving stock (Show, Eq, Generic)

instance Validity QuoteRequest

instance IsComponent QuoteRequest where
  toComponentFields ((QuoteRequest {..})) =
    mconcat
      [ requiredFieldB quoteRequestQuoteReqID,
        optionalFieldB quoteRequestRefSpotDate,
        requiredFieldB quoteRequestProductType,
        optionalFieldB quoteRequestExecutionVenueType,
        optionalGroupB quoteRequestCustomFieldsGroup,
        optionalFieldB quoteRequestDayCount,
        requiredGroupB quoteRequestQuotReqGrpGroup,
        optionalGroupB quoteRequestPartyIDsGroup,
        optionalGroupB quoteRequestRegulatoryTradeIDGroup,
        optionalFieldB quoteRequestOptionDate,
        optionalGroupB quoteRequestOrderAttributeGroup
      ]
  fromComponentFields = do
    quoteRequestQuoteReqID <- requiredFieldP
    quoteRequestRefSpotDate <- optionalFieldP
    quoteRequestProductType <- requiredFieldP
    quoteRequestExecutionVenueType <- optionalFieldP
    quoteRequestCustomFieldsGroup <- optionalGroupP
    quoteRequestDayCount <- optionalFieldP
    quoteRequestQuotReqGrpGroup <- requiredGroupP
    quoteRequestPartyIDsGroup <- optionalGroupP
    quoteRequestRegulatoryTradeIDGroup <- optionalGroupP
    quoteRequestOptionDate <- optionalFieldP
    quoteRequestOrderAttributeGroup <- optionalGroupP
    pure (QuoteRequest {..})

instance IsMessage QuoteRequest where
  messageType Proxy = MsgTypeQuoteRequest

makeQuoteRequest :: QuoteReqID -> (ProductType -> (NonEmpty QuotReqGrpGroupElem -> QuoteRequest))
makeQuoteRequest quoteRequestQuoteReqID quoteRequestProductType quoteRequestQuotReqGrpGroup =
  let quoteRequestRefSpotDate = Nothing
      quoteRequestExecutionVenueType = Nothing
      quoteRequestCustomFieldsGroup = []
      quoteRequestDayCount = Nothing
      quoteRequestPartyIDsGroup = []
      quoteRequestRegulatoryTradeIDGroup = []
      quoteRequestOptionDate = Nothing
      quoteRequestOrderAttributeGroup = []
   in (QuoteRequest {..})
