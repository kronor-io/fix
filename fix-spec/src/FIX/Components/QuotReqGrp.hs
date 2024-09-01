{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Components.QuotReqGrp where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.MsgType
import FIX.Groups.Class
import FIX.Groups.RelatedSymGroupElem
import GHC.Generics (Generic)

-- | ComponentSpec
--   { componentName = "QuotReqGrp"
--   , componentPieces =
--       [ MessagePieceGroup
--           GroupSpec
--             { groupName = "NoRelatedSym"
--             , groupPieces =
--                 [ MessagePieceComponent "Instrument" True
--                 , MessagePieceComponent "FinancingDetails" True
--                 , MessagePieceComponent "UndInstrmtGrp" True
--                 , MessagePieceField "PrevClosePx" False
--                 , MessagePieceField "QuoteRequestType" False
--                 , MessagePieceField "QuoteType" False
--                 , MessagePieceField "TradingSessionID" False
--                 , MessagePieceField "TradingSessionSubID" False
--                 , MessagePieceField "TradeOriginationDate" False
--                 , MessagePieceField "Side" False
--                 , MessagePieceField "QtyType" False
--                 , MessagePieceComponent "OrderQtyData" True
--                 , MessagePieceField "SettlType" False
--                 , MessagePieceField "SettlDate" False
--                 , MessagePieceField "SettlDate2" False
--                 , MessagePieceField "OrderQty2" False
--                 , MessagePieceField "Currency" False
--                 , MessagePieceComponent "Stipulations" True
--                 , MessagePieceField "Account" False
--                 , MessagePieceField "AcctIDSource" False
--                 , MessagePieceField "AccountType" False
--                 , MessagePieceComponent "QuotReqLegsGrp" True
--                 , MessagePieceComponent "QuotQualGrp" True
--                 , MessagePieceField "QuotePriceType" False
--                 , MessagePieceField "OrdType" False
--                 , MessagePieceField "ValidUntilTime" False
--                 , MessagePieceField "ExpireTime" False
--                 , MessagePieceField "TransactTime" False
--                 , MessagePieceComponent "SpreadOrBenchmarkCurveData" True
--                 , MessagePieceField "PriceType" False
--                 , MessagePieceField "Price" False
--                 , MessagePieceField "Price2" False
--                 , MessagePieceComponent "YieldData" True
--                 , MessagePieceComponent "Parties" True
--                 ]
--             }
--           True
--       ]
--   }
data QuotReqGrp = QuotReqGrp {quotReqGrpRelatedSymGroup :: !(NonEmpty RelatedSymGroupElem)}
  deriving stock (Show, Eq, Generic)

instance Validity QuotReqGrp

instance IsComponent QuotReqGrp where
  toComponentFields ((QuotReqGrp {..})) = mconcat [requiredGroupB quotReqGrpRelatedSymGroup]
  fromComponentFields = do
    quotReqGrpRelatedSymGroup <- requiredGroupP
    pure (QuotReqGrp {..})
