{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages.Quote where

import Data.Validity
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
import FIX.Fields.EncodedTextLen
import FIX.Fields.ExDestination
import FIX.Fields.MidPx
import FIX.Fields.MidYield
import FIX.Fields.MinBidSize
import FIX.Fields.MinOfferSize
import FIX.Fields.MktBidPx
import FIX.Fields.MktOfferPx
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
import GHC.Generics (Generic)

-- MessageSpec {messageName = "Quote", messageType = "S", messageCategory = "app", messagePieces = [MessagePieceField "QuoteReqID" False,MessagePieceField "QuoteID" True,MessagePieceField "QuoteRespID" False,MessagePieceField "QuoteType" False,MessagePieceComponent "QuotQualGrp" False,MessagePieceField "QuoteResponseLevel" False,MessagePieceComponent "Parties" False,MessagePieceField "TradingSessionID" False,MessagePieceField "TradingSessionSubID" False,MessagePieceComponent "Instrument" True,MessagePieceComponent "FinancingDetails" False,MessagePieceComponent "UndInstrmtGrp" False,MessagePieceField "Side" False,MessagePieceComponent "OrderQtyData" False,MessagePieceField "SettlType" False,MessagePieceField "SettlDate" False,MessagePieceField "SettlDate2" False,MessagePieceField "OrderQty2" False,MessagePieceField "Currency" False,MessagePieceComponent "Stipulations" False,MessagePieceField "Account" False,MessagePieceField "AcctIDSource" False,MessagePieceField "AccountType" False,MessagePieceComponent "LegQuotGrp" False,MessagePieceField "BidPx" False,MessagePieceField "OfferPx" False,MessagePieceField "MktBidPx" False,MessagePieceField "MktOfferPx" False,MessagePieceField "MinBidSize" False,MessagePieceField "BidSize" False,MessagePieceField "MinOfferSize" False,MessagePieceField "OfferSize" False,MessagePieceField "ValidUntilTime" False,MessagePieceField "BidSpotRate" False,MessagePieceField "OfferSpotRate" False,MessagePieceField "BidForwardPoints" False,MessagePieceField "OfferForwardPoints" False,MessagePieceField "MidPx" False,MessagePieceField "BidYield" False,MessagePieceField "MidYield" False,MessagePieceField "OfferYield" False,MessagePieceField "TransactTime" False,MessagePieceField "OrdType" False,MessagePieceField "BidForwardPoints2" False,MessagePieceField "OfferForwardPoints2" False,MessagePieceField "SettlCurrBidFxRate" False,MessagePieceField "SettlCurrOfferFxRate" False,MessagePieceField "SettlCurrFxRateCalc" False,MessagePieceField "CommType" False,MessagePieceField "Commission" False,MessagePieceField "CustOrderCapacity" False,MessagePieceField "ExDestination" False,MessagePieceField "OrderCapacity" False,MessagePieceField "PriceType" False,MessagePieceComponent "SpreadOrBenchmarkCurveData" False,MessagePieceComponent "YieldData" False,MessagePieceField "Text" False,MessagePieceField "EncodedTextLen" False,MessagePieceField "EncodedText" False]}
data Quote = Quote
  { quoteQuoteReqID :: !(Maybe QuoteReqID),
    quoteQuoteID :: !QuoteID,
    quoteQuoteRespID :: !(Maybe QuoteRespID),
    quoteQuoteType :: !(Maybe QuoteType),
    quoteQuoteResponseLevel :: !(Maybe QuoteResponseLevel),
    quoteTradingSessionID :: !(Maybe TradingSessionID),
    quoteTradingSessionSubID :: !(Maybe TradingSessionSubID),
    quoteSide :: !(Maybe Side),
    quoteSettlType :: !(Maybe SettlType),
    quoteSettlDate :: !(Maybe SettlDate),
    quoteSettlDate2 :: !(Maybe SettlDate2),
    quoteOrderQty2 :: !(Maybe OrderQty2),
    quoteCurrency :: !(Maybe Currency),
    quoteAccount :: !(Maybe Account),
    quoteAcctIDSource :: !(Maybe AcctIDSource),
    quoteAccountType :: !(Maybe AccountType),
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
    quoteText :: !(Maybe Text),
    quoteEncodedTextLen :: !(Maybe EncodedTextLen),
    quoteEncodedText :: !(Maybe EncodedText)
  }
  deriving stock (Show, Eq, Generic)

instance Validity Quote
