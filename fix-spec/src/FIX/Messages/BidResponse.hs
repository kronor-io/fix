{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages.BidResponse where

import Data.Validity
import FIX.Fields.BidID
import FIX.Fields.ClientBidID
import GHC.Generics (Generic)

-- MessageSpec {messageName = "BidResponse", messageType = "l", messageCategory = "app", messagePieces = [MessagePieceField "BidID" False,MessagePieceField "ClientBidID" False,MessagePieceComponent "BidCompRspGrp" True]}
data BidResponse = BidResponse
  { bidResponseBidID :: !(Maybe BidID),
    bidResponseClientBidID :: !(Maybe ClientBidID)
  }
  deriving stock (Show, Eq, Generic)

instance Validity BidResponse
