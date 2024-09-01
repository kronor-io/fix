{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.EventsGroupElem where

import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.EventDate
import FIX.Fields.EventPx
import FIX.Fields.EventText
import FIX.Fields.EventType
import FIX.Fields.MsgType
import FIX.Fields.NoEvents
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "NoEvents"
--   , groupPieces =
--       [ MessagePieceField "EventType" True
--       , MessagePieceField "EventDate" False
--       , MessagePieceField "EventPx" False
--       , MessagePieceField "EventText" False
--       ]
--   }
data EventsGroupElem = EventsGroupElem
  { eventsGroupElemEventType :: !EventType,
    eventsGroupElemEventDate :: !(Maybe EventDate),
    eventsGroupElemEventPx :: !(Maybe EventPx),
    eventsGroupElemEventText :: !(Maybe EventText)
  }
  deriving stock (Show, Eq, Generic)

instance Validity EventsGroupElem

instance IsComponent EventsGroupElem where
  toComponentFields ((EventsGroupElem {..})) =
    mconcat
      [ requiredFieldB eventsGroupElemEventType,
        optionalFieldB eventsGroupElemEventDate,
        optionalFieldB eventsGroupElemEventPx,
        optionalFieldB eventsGroupElemEventText
      ]
  fromComponentFields = do
    eventsGroupElemEventType <- requiredFieldP
    eventsGroupElemEventDate <- optionalFieldP
    eventsGroupElemEventPx <- optionalFieldP
    eventsGroupElemEventText <- optionalFieldP
    pure (EventsGroupElem {..})

instance IsGroupElement EventsGroupElem where
  type GroupNumField EventsGroupElem = NoEvents
  mkGroupNum Proxy = NoEvents
  countGroupNum Proxy = unNoEvents