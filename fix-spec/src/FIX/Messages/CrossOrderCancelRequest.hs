{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages.CrossOrderCancelRequest where

import Data.Validity
import FIX.Fields.CrossID
import FIX.Fields.CrossPrioritization
import FIX.Fields.CrossType
import FIX.Fields.OrderID
import FIX.Fields.OrigCrossID
import FIX.Fields.TransactTime
import GHC.Generics (Generic)

-- MessageSpec {messageName = "CrossOrderCancelRequest", messageType = "u", messageCategory = "app", messagePieces = [MessagePieceField "OrderID" False,MessagePieceField "CrossID" True,MessagePieceField "OrigCrossID" True,MessagePieceField "CrossType" True,MessagePieceField "CrossPrioritization" True,MessagePieceComponent "SideCrossOrdCxlGrp" True,MessagePieceComponent "Instrument" True,MessagePieceComponent "UndInstrmtGrp" False,MessagePieceComponent "InstrmtLegGrp" False,MessagePieceField "TransactTime" True]}
data CrossOrderCancelRequest = CrossOrderCancelRequest
  { crossOrderCancelRequestOrderID :: !(Maybe OrderID),
    crossOrderCancelRequestCrossID :: !CrossID,
    crossOrderCancelRequestOrigCrossID :: !OrigCrossID,
    crossOrderCancelRequestCrossType :: !CrossType,
    crossOrderCancelRequestCrossPrioritization :: !CrossPrioritization,
    crossOrderCancelRequestTransactTime :: !TransactTime
  }
  deriving stock (Show, Eq, Generic)

instance Validity CrossOrderCancelRequest
