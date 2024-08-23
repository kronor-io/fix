{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages.News where

import Data.Validity
import FIX.Fields.EncodedHeadline
import FIX.Fields.EncodedHeadlineLen
import FIX.Fields.Headline
import FIX.Fields.OrigTime
import FIX.Fields.RawData
import FIX.Fields.RawDataLength
import FIX.Fields.URLLink
import FIX.Fields.Urgency
import GHC.Generics (Generic)

-- MessageSpec {messageName = "News", messageType = "B", messageCategory = "app", messagePieces = [MessagePieceField "OrigTime" False,MessagePieceField "Urgency" False,MessagePieceField "Headline" True,MessagePieceField "EncodedHeadlineLen" False,MessagePieceField "EncodedHeadline" False,MessagePieceComponent "RoutingGrp" False,MessagePieceComponent "InstrmtGrp" False,MessagePieceComponent "InstrmtLegGrp" False,MessagePieceComponent "UndInstrmtGrp" False,MessagePieceComponent "LinesOfTextGrp" True,MessagePieceField "URLLink" False,MessagePieceField "RawDataLength" False,MessagePieceField "RawData" False]}
data News = News
  { newsOrigTime :: !(Maybe OrigTime),
    newsUrgency :: !(Maybe Urgency),
    newsHeadline :: !Headline,
    newsEncodedHeadlineLen :: !(Maybe EncodedHeadlineLen),
    newsEncodedHeadline :: !(Maybe EncodedHeadline),
    newsURLLink :: !(Maybe URLLink),
    newsRawDataLength :: !(Maybe RawDataLength),
    newsRawData :: !(Maybe RawData)
  }
  deriving stock (Show, Eq, Generic)

instance Validity News
