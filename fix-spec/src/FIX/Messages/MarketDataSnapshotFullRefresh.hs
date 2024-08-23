{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages.MarketDataSnapshotFullRefresh where

import Data.Validity
import FIX.Fields.ApplQueueDepth
import FIX.Fields.ApplQueueResolution
import FIX.Fields.CorporateAction
import FIX.Fields.FinancialStatus
import FIX.Fields.MDReqID
import FIX.Fields.NetChgPrevDay
import GHC.Generics (Generic)

-- MessageSpec {messageName = "MarketDataSnapshotFullRefresh", messageType = "W", messageCategory = "app", messagePieces = [MessagePieceField "MDReqID" False,MessagePieceComponent "Instrument" True,MessagePieceComponent "UndInstrmtGrp" False,MessagePieceComponent "InstrmtLegGrp" False,MessagePieceField "FinancialStatus" False,MessagePieceField "CorporateAction" False,MessagePieceField "NetChgPrevDay" False,MessagePieceComponent "MDFullGrp" True,MessagePieceField "ApplQueueDepth" False,MessagePieceField "ApplQueueResolution" False]}
data MarketDataSnapshotFullRefresh = MarketDataSnapshotFullRefresh
  { marketDataSnapshotFullRefreshMDReqID :: !(Maybe MDReqID),
    marketDataSnapshotFullRefreshFinancialStatus :: !(Maybe FinancialStatus),
    marketDataSnapshotFullRefreshCorporateAction :: !(Maybe CorporateAction),
    marketDataSnapshotFullRefreshNetChgPrevDay :: !(Maybe NetChgPrevDay),
    marketDataSnapshotFullRefreshApplQueueDepth :: !(Maybe ApplQueueDepth),
    marketDataSnapshotFullRefreshApplQueueResolution :: !(Maybe ApplQueueResolution)
  }
  deriving stock (Show, Eq, Generic)

instance Validity MarketDataSnapshotFullRefresh
