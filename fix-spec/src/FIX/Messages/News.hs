{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages.News where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.Headline
import FIX.Fields.MsgType
import FIX.Groups.Class
import FIX.Groups.NewsLinesOfTextGroupElem
import FIX.Groups.NewsRoutingIDsGroupElem
import FIX.Messages.Class
import GHC.Generics (Generic)

-- | MessageSpec
--   { messageName = "News"
--   , messageType = "B"
--   , messageCategory = "app"
--   , messagePieces =
--       [ MessagePieceField "Headline" True
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "NewsLinesOfText"
--             , groupNumberField = "LinesOfText"
--             , groupPieces =
--                 [ MessagePieceField "Text" True
--                 , MessagePieceField "EncodedText" False
--                 ]
--             }
--           True
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "NewsRoutingIDs"
--             , groupNumberField = "NoRoutingIDs"
--             , groupPieces =
--                 [ MessagePieceField "RoutingType" True
--                 , MessagePieceField "RoutingID" False
--                 ]
--             }
--           False
--       ]
--   }
data News = News
  { newsHeadline :: !Headline,
    newsNewsLinesOfTextGroup :: !(NonEmpty NewsLinesOfTextGroupElem),
    newsNewsRoutingIDsGroup :: ![NewsRoutingIDsGroupElem]
  }
  deriving stock (Show, Eq, Generic)

instance Validity News

instance IsComponent News where
  toComponentFields ((News {..})) =
    mconcat
      [ requiredFieldB newsHeadline,
        requiredGroupB newsNewsLinesOfTextGroup,
        optionalGroupB newsNewsRoutingIDsGroup
      ]
  fromComponentFields = do
    newsHeadline <- requiredFieldP
    newsNewsLinesOfTextGroup <- requiredGroupP
    newsNewsRoutingIDsGroup <- optionalGroupP
    pure (News {..})

instance IsMessage News where
  messageType Proxy = MsgTypeNews

makeNews :: Headline -> (NonEmpty NewsLinesOfTextGroupElem -> News)
makeNews newsHeadline newsNewsLinesOfTextGroup =
  let newsNewsRoutingIDsGroup = []
   in (News {..})
