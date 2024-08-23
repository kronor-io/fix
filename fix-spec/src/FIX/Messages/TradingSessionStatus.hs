{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages.TradingSessionStatus where

import Data.Validity
import FIX.Fields.EncodedText
import FIX.Fields.EncodedTextLen
import FIX.Fields.Text
import FIX.Fields.TotalVolumeTraded
import FIX.Fields.TradSesCloseTime
import FIX.Fields.TradSesEndTime
import FIX.Fields.TradSesMethod
import FIX.Fields.TradSesMode
import FIX.Fields.TradSesOpenTime
import FIX.Fields.TradSesPreCloseTime
import FIX.Fields.TradSesReqID
import FIX.Fields.TradSesStartTime
import FIX.Fields.TradSesStatus
import FIX.Fields.TradSesStatusRejReason
import FIX.Fields.TradingSessionID
import FIX.Fields.TradingSessionSubID
import FIX.Fields.UnsolicitedIndicator
import GHC.Generics (Generic)

-- MessageSpec {messageName = "TradingSessionStatus", messageType = "h", messageCategory = "app", messagePieces = [MessagePieceField "TradSesReqID" False,MessagePieceField "TradingSessionID" True,MessagePieceField "TradingSessionSubID" False,MessagePieceField "TradSesMethod" False,MessagePieceField "TradSesMode" False,MessagePieceField "UnsolicitedIndicator" False,MessagePieceField "TradSesStatus" True,MessagePieceField "TradSesStatusRejReason" False,MessagePieceField "TradSesStartTime" False,MessagePieceField "TradSesOpenTime" False,MessagePieceField "TradSesPreCloseTime" False,MessagePieceField "TradSesCloseTime" False,MessagePieceField "TradSesEndTime" False,MessagePieceField "TotalVolumeTraded" False,MessagePieceField "Text" False,MessagePieceField "EncodedTextLen" False,MessagePieceField "EncodedText" False]}
data TradingSessionStatus = TradingSessionStatus
  { tradingSessionStatusTradSesReqID :: !(Maybe TradSesReqID),
    tradingSessionStatusTradingSessionID :: !TradingSessionID,
    tradingSessionStatusTradingSessionSubID :: !(Maybe TradingSessionSubID),
    tradingSessionStatusTradSesMethod :: !(Maybe TradSesMethod),
    tradingSessionStatusTradSesMode :: !(Maybe TradSesMode),
    tradingSessionStatusUnsolicitedIndicator :: !(Maybe UnsolicitedIndicator),
    tradingSessionStatusTradSesStatus :: !TradSesStatus,
    tradingSessionStatusTradSesStatusRejReason :: !(Maybe TradSesStatusRejReason),
    tradingSessionStatusTradSesStartTime :: !(Maybe TradSesStartTime),
    tradingSessionStatusTradSesOpenTime :: !(Maybe TradSesOpenTime),
    tradingSessionStatusTradSesPreCloseTime :: !(Maybe TradSesPreCloseTime),
    tradingSessionStatusTradSesCloseTime :: !(Maybe TradSesCloseTime),
    tradingSessionStatusTradSesEndTime :: !(Maybe TradSesEndTime),
    tradingSessionStatusTotalVolumeTraded :: !(Maybe TotalVolumeTraded),
    tradingSessionStatusText :: !(Maybe Text),
    tradingSessionStatusEncodedTextLen :: !(Maybe EncodedTextLen),
    tradingSessionStatusEncodedText :: !(Maybe EncodedText)
  }
  deriving stock (Show, Eq, Generic)

instance Validity TradingSessionStatus
