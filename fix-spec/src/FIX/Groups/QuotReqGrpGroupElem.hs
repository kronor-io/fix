{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.QuotReqGrpGroupElem where

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
import FIX.Fields.Currency
import FIX.Fields.ExpireTime
import FIX.Fields.MsgType
import FIX.Fields.NoRelatedSym
import FIX.Fields.OrdType
import FIX.Fields.OrderQty2
import FIX.Fields.PrevClosePx
import FIX.Fields.Price
import FIX.Fields.Price2
import FIX.Fields.PriceType
import FIX.Fields.QtyType
import FIX.Fields.QuotePriceType
import FIX.Fields.QuoteRequestType
import FIX.Fields.QuoteType
import FIX.Fields.SettlDate
import FIX.Fields.SettlDate2
import FIX.Fields.SettlType
import FIX.Fields.Side
import FIX.Fields.TradeOriginationDate
import FIX.Fields.TradingSessionID
import FIX.Fields.TradingSessionSubID
import FIX.Fields.TransactTime
import FIX.Fields.ValidUntilTime
import FIX.Groups.Class
import FIX.Groups.PartiesGroupElem
import FIX.Groups.QuotQualGrpGroupElem
import FIX.Groups.QuotReqLegsGrpGroupElem
import FIX.Groups.StipulationsGroupElem
import FIX.Groups.UndInstrmtGrpGroupElem
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "QuotReqGrp"
--   , groupNumberField = "NoRelatedSym"
--   , groupPieces =
--       [ MessagePieceComponent "Instrument" True
--       , MessagePieceComponent "FinancingDetails" True
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "UndInstrmtGrp"
--             , groupNumberField = "NoUnderlyings"
--             , groupPieces =
--                 [ MessagePieceComponent "UnderlyingInstrument" True ]
--             }
--           False
--       , MessagePieceField "PrevClosePx" False
--       , MessagePieceField "QuoteRequestType" False
--       , MessagePieceField "QuoteType" False
--       , MessagePieceField "TradingSessionID" False
--       , MessagePieceField "TradingSessionSubID" False
--       , MessagePieceField "TradeOriginationDate" False
--       , MessagePieceField "Side" False
--       , MessagePieceField "QtyType" False
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
--             { groupName = "QuotReqLegsGrp"
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
--                 , MessagePieceComponent "LegBenchmarkCurveData" True
--                 ]
--             }
--           False
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "QuotQualGrp"
--             , groupNumberField = "NoQuoteQualifiers"
--             , groupPieces = [ MessagePieceField "QuoteQualifier" True ]
--             }
--           False
--       , MessagePieceField "QuotePriceType" False
--       , MessagePieceField "OrdType" False
--       , MessagePieceField "ValidUntilTime" False
--       , MessagePieceField "ExpireTime" False
--       , MessagePieceField "TransactTime" False
--       , MessagePieceComponent "SpreadOrBenchmarkCurveData" True
--       , MessagePieceField "PriceType" False
--       , MessagePieceField "Price" False
--       , MessagePieceField "Price2" False
--       , MessagePieceComponent "YieldData" True
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
--       ]
--   }
data QuotReqGrpGroupElem = QuotReqGrpGroupElem
  { quotReqGrpGroupElemInstrument :: !Instrument,
    quotReqGrpGroupElemFinancingDetails :: !FinancingDetails,
    quotReqGrpGroupElemUndInstrmtGrpGroup :: ![UndInstrmtGrpGroupElem],
    quotReqGrpGroupElemPrevClosePx :: !(Maybe PrevClosePx),
    quotReqGrpGroupElemQuoteRequestType :: !(Maybe QuoteRequestType),
    quotReqGrpGroupElemQuoteType :: !(Maybe QuoteType),
    quotReqGrpGroupElemTradingSessionID :: !(Maybe TradingSessionID),
    quotReqGrpGroupElemTradingSessionSubID :: !(Maybe TradingSessionSubID),
    quotReqGrpGroupElemTradeOriginationDate :: !(Maybe TradeOriginationDate),
    quotReqGrpGroupElemSide :: !(Maybe Side),
    quotReqGrpGroupElemQtyType :: !(Maybe QtyType),
    quotReqGrpGroupElemOrderQtyData :: !OrderQtyData,
    quotReqGrpGroupElemSettlType :: !(Maybe SettlType),
    quotReqGrpGroupElemSettlDate :: !(Maybe SettlDate),
    quotReqGrpGroupElemSettlDate2 :: !(Maybe SettlDate2),
    quotReqGrpGroupElemOrderQty2 :: !(Maybe OrderQty2),
    quotReqGrpGroupElemCurrency :: !(Maybe Currency),
    quotReqGrpGroupElemStipulationsGroup :: ![StipulationsGroupElem],
    quotReqGrpGroupElemAccount :: !(Maybe Account),
    quotReqGrpGroupElemAcctIDSource :: !(Maybe AcctIDSource),
    quotReqGrpGroupElemAccountType :: !(Maybe AccountType),
    quotReqGrpGroupElemQuotReqLegsGrpGroup :: ![QuotReqLegsGrpGroupElem],
    quotReqGrpGroupElemQuotQualGrpGroup :: ![QuotQualGrpGroupElem],
    quotReqGrpGroupElemQuotePriceType :: !(Maybe QuotePriceType),
    quotReqGrpGroupElemOrdType :: !(Maybe OrdType),
    quotReqGrpGroupElemValidUntilTime :: !(Maybe ValidUntilTime),
    quotReqGrpGroupElemExpireTime :: !(Maybe ExpireTime),
    quotReqGrpGroupElemTransactTime :: !(Maybe TransactTime),
    quotReqGrpGroupElemSpreadOrBenchmarkCurveData :: !SpreadOrBenchmarkCurveData,
    quotReqGrpGroupElemPriceType :: !(Maybe PriceType),
    quotReqGrpGroupElemPrice :: !(Maybe Price),
    quotReqGrpGroupElemPrice2 :: !(Maybe Price2),
    quotReqGrpGroupElemYieldData :: !YieldData,
    quotReqGrpGroupElemPartiesGroup :: ![PartiesGroupElem]
  }
  deriving stock (Show, Eq, Generic)

instance Validity QuotReqGrpGroupElem

instance IsComponent QuotReqGrpGroupElem where
  toComponentFields ((QuotReqGrpGroupElem {..})) =
    mconcat
      [ requiredComponentB quotReqGrpGroupElemInstrument,
        requiredComponentB quotReqGrpGroupElemFinancingDetails,
        optionalGroupB quotReqGrpGroupElemUndInstrmtGrpGroup,
        optionalFieldB quotReqGrpGroupElemPrevClosePx,
        optionalFieldB quotReqGrpGroupElemQuoteRequestType,
        optionalFieldB quotReqGrpGroupElemQuoteType,
        optionalFieldB quotReqGrpGroupElemTradingSessionID,
        optionalFieldB quotReqGrpGroupElemTradingSessionSubID,
        optionalFieldB quotReqGrpGroupElemTradeOriginationDate,
        optionalFieldB quotReqGrpGroupElemSide,
        optionalFieldB quotReqGrpGroupElemQtyType,
        requiredComponentB quotReqGrpGroupElemOrderQtyData,
        optionalFieldB quotReqGrpGroupElemSettlType,
        optionalFieldB quotReqGrpGroupElemSettlDate,
        optionalFieldB quotReqGrpGroupElemSettlDate2,
        optionalFieldB quotReqGrpGroupElemOrderQty2,
        optionalFieldB quotReqGrpGroupElemCurrency,
        optionalGroupB quotReqGrpGroupElemStipulationsGroup,
        optionalFieldB quotReqGrpGroupElemAccount,
        optionalFieldB quotReqGrpGroupElemAcctIDSource,
        optionalFieldB quotReqGrpGroupElemAccountType,
        optionalGroupB quotReqGrpGroupElemQuotReqLegsGrpGroup,
        optionalGroupB quotReqGrpGroupElemQuotQualGrpGroup,
        optionalFieldB quotReqGrpGroupElemQuotePriceType,
        optionalFieldB quotReqGrpGroupElemOrdType,
        optionalFieldB quotReqGrpGroupElemValidUntilTime,
        optionalFieldB quotReqGrpGroupElemExpireTime,
        optionalFieldB quotReqGrpGroupElemTransactTime,
        requiredComponentB quotReqGrpGroupElemSpreadOrBenchmarkCurveData,
        optionalFieldB quotReqGrpGroupElemPriceType,
        optionalFieldB quotReqGrpGroupElemPrice,
        optionalFieldB quotReqGrpGroupElemPrice2,
        requiredComponentB quotReqGrpGroupElemYieldData,
        optionalGroupB quotReqGrpGroupElemPartiesGroup
      ]
  fromComponentFields = do
    quotReqGrpGroupElemInstrument <- requiredComponentP
    quotReqGrpGroupElemFinancingDetails <- requiredComponentP
    quotReqGrpGroupElemUndInstrmtGrpGroup <- optionalGroupP
    quotReqGrpGroupElemPrevClosePx <- optionalFieldP
    quotReqGrpGroupElemQuoteRequestType <- optionalFieldP
    quotReqGrpGroupElemQuoteType <- optionalFieldP
    quotReqGrpGroupElemTradingSessionID <- optionalFieldP
    quotReqGrpGroupElemTradingSessionSubID <- optionalFieldP
    quotReqGrpGroupElemTradeOriginationDate <- optionalFieldP
    quotReqGrpGroupElemSide <- optionalFieldP
    quotReqGrpGroupElemQtyType <- optionalFieldP
    quotReqGrpGroupElemOrderQtyData <- requiredComponentP
    quotReqGrpGroupElemSettlType <- optionalFieldP
    quotReqGrpGroupElemSettlDate <- optionalFieldP
    quotReqGrpGroupElemSettlDate2 <- optionalFieldP
    quotReqGrpGroupElemOrderQty2 <- optionalFieldP
    quotReqGrpGroupElemCurrency <- optionalFieldP
    quotReqGrpGroupElemStipulationsGroup <- optionalGroupP
    quotReqGrpGroupElemAccount <- optionalFieldP
    quotReqGrpGroupElemAcctIDSource <- optionalFieldP
    quotReqGrpGroupElemAccountType <- optionalFieldP
    quotReqGrpGroupElemQuotReqLegsGrpGroup <- optionalGroupP
    quotReqGrpGroupElemQuotQualGrpGroup <- optionalGroupP
    quotReqGrpGroupElemQuotePriceType <- optionalFieldP
    quotReqGrpGroupElemOrdType <- optionalFieldP
    quotReqGrpGroupElemValidUntilTime <- optionalFieldP
    quotReqGrpGroupElemExpireTime <- optionalFieldP
    quotReqGrpGroupElemTransactTime <- optionalFieldP
    quotReqGrpGroupElemSpreadOrBenchmarkCurveData <- requiredComponentP
    quotReqGrpGroupElemPriceType <- optionalFieldP
    quotReqGrpGroupElemPrice <- optionalFieldP
    quotReqGrpGroupElemPrice2 <- optionalFieldP
    quotReqGrpGroupElemYieldData <- requiredComponentP
    quotReqGrpGroupElemPartiesGroup <- optionalGroupP
    pure (QuotReqGrpGroupElem {..})

instance IsGroupElement QuotReqGrpGroupElem where
  type GroupNumField QuotReqGrpGroupElem = NoRelatedSym
  mkGroupNum Proxy = NoRelatedSym
  countGroupNum Proxy = unNoRelatedSym
