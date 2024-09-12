{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages.Quote where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Components.FinancingDetails
import FIX.Components.Instrument
import FIX.Components.OrderQtyData
import FIX.Components.SpreadOrBenchmarkCurveData
import FIX.Components.YieldData
import FIX.Fields.Account
import FIX.Fields.AccountType
import FIX.Fields.AcctIDSource
import FIX.Fields.BidForwardPoints
import FIX.Fields.BidForwardPoints2
import FIX.Fields.BidPx
import FIX.Fields.BidSize
import FIX.Fields.BidSpotRate
import FIX.Fields.BidYield
import FIX.Fields.CommType
import FIX.Fields.Commission
import FIX.Fields.Currency
import FIX.Fields.CustOrderCapacity
import FIX.Fields.EncodedText
import FIX.Fields.ExDestination
import FIX.Fields.MidPx
import FIX.Fields.MidYield
import FIX.Fields.MinBidSize
import FIX.Fields.MinOfferSize
import FIX.Fields.MktBidPx
import FIX.Fields.MktOfferPx
import FIX.Fields.MsgType
import FIX.Fields.OfferForwardPoints
import FIX.Fields.OfferForwardPoints2
import FIX.Fields.OfferPx
import FIX.Fields.OfferSize
import FIX.Fields.OfferSpotRate
import FIX.Fields.OfferYield
import FIX.Fields.OrdType
import FIX.Fields.OrderCapacity
import FIX.Fields.OrderQty2
import FIX.Fields.PriceType
import FIX.Fields.QuoteID
import FIX.Fields.QuoteReqID
import FIX.Fields.QuoteRespID
import FIX.Fields.QuoteResponseLevel
import FIX.Fields.QuoteType
import FIX.Fields.SettlCurrBidFxRate
import FIX.Fields.SettlCurrFxRateCalc
import FIX.Fields.SettlCurrOfferFxRate
import FIX.Fields.SettlDate
import FIX.Fields.SettlDate2
import FIX.Fields.SettlType
import FIX.Fields.Side
import FIX.Fields.Text
import FIX.Fields.TradingSessionID
import FIX.Fields.TradingSessionSubID
import FIX.Fields.TransactTime
import FIX.Fields.ValidUntilTime
import FIX.Groups.Class
import FIX.Groups.LegQuotGrpGroupElem
import FIX.Groups.PartiesGroupElem
import FIX.Groups.QuotQualGrpGroupElem
import FIX.Groups.StipulationsGroupElem
import FIX.Groups.UndInstrmtGrpGroupElem
import FIX.Messages.Class
import GHC.Generics (Generic)

-- | MessageSpec
--   { messageName = "Quote"
--   , messageType = "S"
--   , messageCategory = "app"
--   , messagePieces =
--       [ MessagePieceField "QuoteReqID" False
--       , MessagePieceField "QuoteID" True
--       , MessagePieceField "QuoteRespID" False
--       , MessagePieceField "QuoteType" False
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "QuotQualGrp"
--             , groupNumberField = "NoQuoteQualifiers"
--             , groupPieces = [ MessagePieceField "QuoteQualifier" True ]
--             }
--           False
--       , MessagePieceField "QuoteResponseLevel" False
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "Parties"
--             , groupNumberField = "NoPartyIDs"
--             , groupPieces =
--                 [ MessagePieceField "PartyID" True
--                 , MessagePieceField "PartyIDSource" False
--                 , MessagePieceField "PartyRole" False
--                 , MessagePieceGroup
--                     GroupSpec
--                       { groupName = "PtysSubGrp"
--                       , groupNumberField = "NoPartySubIDs"
--                       , groupPieces =
--                           [ MessagePieceField "PartySubID" True
--                           , MessagePieceField "PartySubIDType" False
--                           ]
--                       }
--                     False
--                 ]
--             }
--           False
--       , MessagePieceField "TradingSessionID" False
--       , MessagePieceField "TradingSessionSubID" False
--       , MessagePieceComponent "Instrument" True
--       , MessagePieceComponent "FinancingDetails" True
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "UndInstrmtGrp"
--             , groupNumberField = "NoUnderlyings"
--             , groupPieces =
--                 [ MessagePieceComponent "UnderlyingInstrument" True ]
--             }
--           False
--       , MessagePieceField "Side" False
--       , MessagePieceComponent "OrderQtyData" True
--       , MessagePieceField "SettlType" False
--       , MessagePieceField "SettlDate" False
--       , MessagePieceField "SettlDate2" False
--       , MessagePieceField "OrderQty2" False
--       , MessagePieceField "Currency" False
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "Stipulations"
--             , groupNumberField = "NoStipulations"
--             , groupPieces =
--                 [ MessagePieceField "StipulationType" True
--                 , MessagePieceField "StipulationValue" False
--                 ]
--             }
--           False
--       , MessagePieceField "Account" False
--       , MessagePieceField "AcctIDSource" False
--       , MessagePieceField "AccountType" False
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "LegQuotGrp"
--             , groupNumberField = "NoLegs"
--             , groupPieces =
--                 [ MessagePieceComponent "InstrumentLeg" True
--                 , MessagePieceField "LegQty" False
--                 , MessagePieceField "LegSwapType" False
--                 , MessagePieceField "LegSettlType" False
--                 , MessagePieceField "LegSettlDate" False
--                 , MessagePieceGroup
--                     GroupSpec
--                       { groupName = "LegStipulations"
--                       , groupNumberField = "NoLegStipulations"
--                       , groupPieces =
--                           [ MessagePieceField "LegStipulationType" True
--                           , MessagePieceField "LegStipulationValue" False
--                           ]
--                       }
--                     False
--                 , MessagePieceGroup
--                     GroupSpec
--                       { groupName = "NestedParties"
--                       , groupNumberField = "NoNestedPartyIDs"
--                       , groupPieces =
--                           [ MessagePieceField "NestedPartyID" True
--                           , MessagePieceField "NestedPartyIDSource" False
--                           , MessagePieceField "NestedPartyRole" False
--                           , MessagePieceGroup
--                               GroupSpec
--                                 { groupName = "NstdPtysSubGrp"
--                                 , groupNumberField = "NoNestedPartySubIDs"
--                                 , groupPieces =
--                                     [ MessagePieceField "NestedPartySubID" True
--                                     , MessagePieceField "NestedPartySubIDType" False
--                                     ]
--                                 }
--                               False
--                           ]
--                       }
--                     False
--                 , MessagePieceField "LegPriceType" False
--                 , MessagePieceField "LegBidPx" False
--                 , MessagePieceField "LegOfferPx" False
--                 , MessagePieceComponent "LegBenchmarkCurveData" True
--                 ]
--             }
--           False
--       , MessagePieceField "BidPx" False
--       , MessagePieceField "OfferPx" False
--       , MessagePieceField "MktBidPx" False
--       , MessagePieceField "MktOfferPx" False
--       , MessagePieceField "MinBidSize" False
--       , MessagePieceField "BidSize" False
--       , MessagePieceField "MinOfferSize" False
--       , MessagePieceField "OfferSize" False
--       , MessagePieceField "ValidUntilTime" False
--       , MessagePieceField "BidSpotRate" False
--       , MessagePieceField "OfferSpotRate" False
--       , MessagePieceField "BidForwardPoints" False
--       , MessagePieceField "OfferForwardPoints" False
--       , MessagePieceField "MidPx" False
--       , MessagePieceField "BidYield" False
--       , MessagePieceField "MidYield" False
--       , MessagePieceField "OfferYield" False
--       , MessagePieceField "TransactTime" False
--       , MessagePieceField "OrdType" False
--       , MessagePieceField "BidForwardPoints2" False
--       , MessagePieceField "OfferForwardPoints2" False
--       , MessagePieceField "SettlCurrBidFxRate" False
--       , MessagePieceField "SettlCurrOfferFxRate" False
--       , MessagePieceField "SettlCurrFxRateCalc" False
--       , MessagePieceField "CommType" False
--       , MessagePieceField "Commission" False
--       , MessagePieceField "CustOrderCapacity" False
--       , MessagePieceField "ExDestination" False
--       , MessagePieceField "OrderCapacity" False
--       , MessagePieceField "PriceType" False
--       , MessagePieceComponent "SpreadOrBenchmarkCurveData" True
--       , MessagePieceComponent "YieldData" True
--       , MessagePieceField "Text" False
--       , MessagePieceField "EncodedText" False
--       ]
--   }
data Quote = Quote
  { quoteQuoteReqID :: !(Maybe QuoteReqID),
    quoteQuoteID :: !QuoteID,
    quoteQuoteRespID :: !(Maybe QuoteRespID),
    quoteQuoteType :: !(Maybe QuoteType),
    quoteQuotQualGrpGroup :: ![QuotQualGrpGroupElem],
    quoteQuoteResponseLevel :: !(Maybe QuoteResponseLevel),
    quotePartiesGroup :: ![PartiesGroupElem],
    quoteTradingSessionID :: !(Maybe TradingSessionID),
    quoteTradingSessionSubID :: !(Maybe TradingSessionSubID),
    quoteInstrument :: !Instrument,
    quoteFinancingDetails :: !FinancingDetails,
    quoteUndInstrmtGrpGroup :: ![UndInstrmtGrpGroupElem],
    quoteSide :: !(Maybe Side),
    quoteOrderQtyData :: !OrderQtyData,
    quoteSettlType :: !(Maybe SettlType),
    quoteSettlDate :: !(Maybe SettlDate),
    quoteSettlDate2 :: !(Maybe SettlDate2),
    quoteOrderQty2 :: !(Maybe OrderQty2),
    quoteCurrency :: !(Maybe Currency),
    quoteStipulationsGroup :: ![StipulationsGroupElem],
    quoteAccount :: !(Maybe Account),
    quoteAcctIDSource :: !(Maybe AcctIDSource),
    quoteAccountType :: !(Maybe AccountType),
    quoteLegQuotGrpGroup :: ![LegQuotGrpGroupElem],
    quoteBidPx :: !(Maybe BidPx),
    quoteOfferPx :: !(Maybe OfferPx),
    quoteMktBidPx :: !(Maybe MktBidPx),
    quoteMktOfferPx :: !(Maybe MktOfferPx),
    quoteMinBidSize :: !(Maybe MinBidSize),
    quoteBidSize :: !(Maybe BidSize),
    quoteMinOfferSize :: !(Maybe MinOfferSize),
    quoteOfferSize :: !(Maybe OfferSize),
    quoteValidUntilTime :: !(Maybe ValidUntilTime),
    quoteBidSpotRate :: !(Maybe BidSpotRate),
    quoteOfferSpotRate :: !(Maybe OfferSpotRate),
    quoteBidForwardPoints :: !(Maybe BidForwardPoints),
    quoteOfferForwardPoints :: !(Maybe OfferForwardPoints),
    quoteMidPx :: !(Maybe MidPx),
    quoteBidYield :: !(Maybe BidYield),
    quoteMidYield :: !(Maybe MidYield),
    quoteOfferYield :: !(Maybe OfferYield),
    quoteTransactTime :: !(Maybe TransactTime),
    quoteOrdType :: !(Maybe OrdType),
    quoteBidForwardPoints2 :: !(Maybe BidForwardPoints2),
    quoteOfferForwardPoints2 :: !(Maybe OfferForwardPoints2),
    quoteSettlCurrBidFxRate :: !(Maybe SettlCurrBidFxRate),
    quoteSettlCurrOfferFxRate :: !(Maybe SettlCurrOfferFxRate),
    quoteSettlCurrFxRateCalc :: !(Maybe SettlCurrFxRateCalc),
    quoteCommType :: !(Maybe CommType),
    quoteCommission :: !(Maybe Commission),
    quoteCustOrderCapacity :: !(Maybe CustOrderCapacity),
    quoteExDestination :: !(Maybe ExDestination),
    quoteOrderCapacity :: !(Maybe OrderCapacity),
    quotePriceType :: !(Maybe PriceType),
    quoteSpreadOrBenchmarkCurveData :: !SpreadOrBenchmarkCurveData,
    quoteYieldData :: !YieldData,
    quoteText :: !(Maybe Text),
    quoteEncodedText :: !(Maybe EncodedText)
  }
  deriving stock (Show, Eq, Generic)

instance Validity Quote

instance IsComponent Quote where
  toComponentFields ((Quote {..})) =
    mconcat
      [ optionalFieldB quoteQuoteReqID,
        requiredFieldB quoteQuoteID,
        optionalFieldB quoteQuoteRespID,
        optionalFieldB quoteQuoteType,
        optionalGroupB quoteQuotQualGrpGroup,
        optionalFieldB quoteQuoteResponseLevel,
        optionalGroupB quotePartiesGroup,
        optionalFieldB quoteTradingSessionID,
        optionalFieldB quoteTradingSessionSubID,
        requiredComponentB quoteInstrument,
        requiredComponentB quoteFinancingDetails,
        optionalGroupB quoteUndInstrmtGrpGroup,
        optionalFieldB quoteSide,
        requiredComponentB quoteOrderQtyData,
        optionalFieldB quoteSettlType,
        optionalFieldB quoteSettlDate,
        optionalFieldB quoteSettlDate2,
        optionalFieldB quoteOrderQty2,
        optionalFieldB quoteCurrency,
        optionalGroupB quoteStipulationsGroup,
        optionalFieldB quoteAccount,
        optionalFieldB quoteAcctIDSource,
        optionalFieldB quoteAccountType,
        optionalGroupB quoteLegQuotGrpGroup,
        optionalFieldB quoteBidPx,
        optionalFieldB quoteOfferPx,
        optionalFieldB quoteMktBidPx,
        optionalFieldB quoteMktOfferPx,
        optionalFieldB quoteMinBidSize,
        optionalFieldB quoteBidSize,
        optionalFieldB quoteMinOfferSize,
        optionalFieldB quoteOfferSize,
        optionalFieldB quoteValidUntilTime,
        optionalFieldB quoteBidSpotRate,
        optionalFieldB quoteOfferSpotRate,
        optionalFieldB quoteBidForwardPoints,
        optionalFieldB quoteOfferForwardPoints,
        optionalFieldB quoteMidPx,
        optionalFieldB quoteBidYield,
        optionalFieldB quoteMidYield,
        optionalFieldB quoteOfferYield,
        optionalFieldB quoteTransactTime,
        optionalFieldB quoteOrdType,
        optionalFieldB quoteBidForwardPoints2,
        optionalFieldB quoteOfferForwardPoints2,
        optionalFieldB quoteSettlCurrBidFxRate,
        optionalFieldB quoteSettlCurrOfferFxRate,
        optionalFieldB quoteSettlCurrFxRateCalc,
        optionalFieldB quoteCommType,
        optionalFieldB quoteCommission,
        optionalFieldB quoteCustOrderCapacity,
        optionalFieldB quoteExDestination,
        optionalFieldB quoteOrderCapacity,
        optionalFieldB quotePriceType,
        requiredComponentB quoteSpreadOrBenchmarkCurveData,
        requiredComponentB quoteYieldData,
        optionalFieldB quoteText,
        optionalFieldB quoteEncodedText
      ]
  fromComponentFields = do
    quoteQuoteReqID <- optionalFieldP
    quoteQuoteID <- requiredFieldP
    quoteQuoteRespID <- optionalFieldP
    quoteQuoteType <- optionalFieldP
    quoteQuotQualGrpGroup <- optionalGroupP
    quoteQuoteResponseLevel <- optionalFieldP
    quotePartiesGroup <- optionalGroupP
    quoteTradingSessionID <- optionalFieldP
    quoteTradingSessionSubID <- optionalFieldP
    quoteInstrument <- requiredComponentP
    quoteFinancingDetails <- requiredComponentP
    quoteUndInstrmtGrpGroup <- optionalGroupP
    quoteSide <- optionalFieldP
    quoteOrderQtyData <- requiredComponentP
    quoteSettlType <- optionalFieldP
    quoteSettlDate <- optionalFieldP
    quoteSettlDate2 <- optionalFieldP
    quoteOrderQty2 <- optionalFieldP
    quoteCurrency <- optionalFieldP
    quoteStipulationsGroup <- optionalGroupP
    quoteAccount <- optionalFieldP
    quoteAcctIDSource <- optionalFieldP
    quoteAccountType <- optionalFieldP
    quoteLegQuotGrpGroup <- optionalGroupP
    quoteBidPx <- optionalFieldP
    quoteOfferPx <- optionalFieldP
    quoteMktBidPx <- optionalFieldP
    quoteMktOfferPx <- optionalFieldP
    quoteMinBidSize <- optionalFieldP
    quoteBidSize <- optionalFieldP
    quoteMinOfferSize <- optionalFieldP
    quoteOfferSize <- optionalFieldP
    quoteValidUntilTime <- optionalFieldP
    quoteBidSpotRate <- optionalFieldP
    quoteOfferSpotRate <- optionalFieldP
    quoteBidForwardPoints <- optionalFieldP
    quoteOfferForwardPoints <- optionalFieldP
    quoteMidPx <- optionalFieldP
    quoteBidYield <- optionalFieldP
    quoteMidYield <- optionalFieldP
    quoteOfferYield <- optionalFieldP
    quoteTransactTime <- optionalFieldP
    quoteOrdType <- optionalFieldP
    quoteBidForwardPoints2 <- optionalFieldP
    quoteOfferForwardPoints2 <- optionalFieldP
    quoteSettlCurrBidFxRate <- optionalFieldP
    quoteSettlCurrOfferFxRate <- optionalFieldP
    quoteSettlCurrFxRateCalc <- optionalFieldP
    quoteCommType <- optionalFieldP
    quoteCommission <- optionalFieldP
    quoteCustOrderCapacity <- optionalFieldP
    quoteExDestination <- optionalFieldP
    quoteOrderCapacity <- optionalFieldP
    quotePriceType <- optionalFieldP
    quoteSpreadOrBenchmarkCurveData <- requiredComponentP
    quoteYieldData <- requiredComponentP
    quoteText <- optionalFieldP
    quoteEncodedText <- optionalFieldP
    pure (Quote {..})

instance IsMessage Quote where
  messageType Proxy = MsgTypeQuote

makeQuote :: QuoteID -> (Instrument -> (FinancingDetails -> (OrderQtyData -> (SpreadOrBenchmarkCurveData -> (YieldData -> Quote)))))
makeQuote quoteQuoteID quoteInstrument quoteFinancingDetails quoteOrderQtyData quoteSpreadOrBenchmarkCurveData quoteYieldData =
  let quoteQuoteReqID = Nothing
      quoteQuoteRespID = Nothing
      quoteQuoteType = Nothing
      quoteQuotQualGrpGroup = []
      quoteQuoteResponseLevel = Nothing
      quotePartiesGroup = []
      quoteTradingSessionID = Nothing
      quoteTradingSessionSubID = Nothing
      quoteUndInstrmtGrpGroup = []
      quoteSide = Nothing
      quoteSettlType = Nothing
      quoteSettlDate = Nothing
      quoteSettlDate2 = Nothing
      quoteOrderQty2 = Nothing
      quoteCurrency = Nothing
      quoteStipulationsGroup = []
      quoteAccount = Nothing
      quoteAcctIDSource = Nothing
      quoteAccountType = Nothing
      quoteLegQuotGrpGroup = []
      quoteBidPx = Nothing
      quoteOfferPx = Nothing
      quoteMktBidPx = Nothing
      quoteMktOfferPx = Nothing
      quoteMinBidSize = Nothing
      quoteBidSize = Nothing
      quoteMinOfferSize = Nothing
      quoteOfferSize = Nothing
      quoteValidUntilTime = Nothing
      quoteBidSpotRate = Nothing
      quoteOfferSpotRate = Nothing
      quoteBidForwardPoints = Nothing
      quoteOfferForwardPoints = Nothing
      quoteMidPx = Nothing
      quoteBidYield = Nothing
      quoteMidYield = Nothing
      quoteOfferYield = Nothing
      quoteTransactTime = Nothing
      quoteOrdType = Nothing
      quoteBidForwardPoints2 = Nothing
      quoteOfferForwardPoints2 = Nothing
      quoteSettlCurrBidFxRate = Nothing
      quoteSettlCurrOfferFxRate = Nothing
      quoteSettlCurrFxRateCalc = Nothing
      quoteCommType = Nothing
      quoteCommission = Nothing
      quoteCustOrderCapacity = Nothing
      quoteExDestination = Nothing
      quoteOrderCapacity = Nothing
      quotePriceType = Nothing
      quoteText = Nothing
      quoteEncodedText = Nothing
   in (Quote {..})