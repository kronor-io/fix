{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.NewOrderSingleTradingSessionsGroupElem where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.MsgType
import FIX.Fields.NoTradingSessions
import FIX.Fields.TradingSessionID
import FIX.Fields.TradingSessionSubID
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "NewOrderSingleTradingSessions"
--   , groupNumberField = "NoTradingSessions"
--   , groupPieces =
--       [ MessagePieceField "TradingSessionID" True
--       , MessagePieceField "TradingSessionSubID" False
--       ]
--   }
data NewOrderSingleTradingSessionsGroupElem = NewOrderSingleTradingSessionsGroupElem
  { newOrderSingleTradingSessionsGroupElemTradingSessionID :: !TradingSessionID,
    newOrderSingleTradingSessionsGroupElemTradingSessionSubID :: !(Maybe TradingSessionSubID)
  }
  deriving stock (Show, Eq, Generic)

instance Validity NewOrderSingleTradingSessionsGroupElem

instance IsComponent NewOrderSingleTradingSessionsGroupElem where
  toComponentFields ((NewOrderSingleTradingSessionsGroupElem {..})) =
    mconcat
      [ requiredFieldB newOrderSingleTradingSessionsGroupElemTradingSessionID,
        optionalFieldB newOrderSingleTradingSessionsGroupElemTradingSessionSubID
      ]
  fromComponentFields = do
    newOrderSingleTradingSessionsGroupElemTradingSessionID <- requiredFieldP
    newOrderSingleTradingSessionsGroupElemTradingSessionSubID <- optionalFieldP
    pure (NewOrderSingleTradingSessionsGroupElem {..})

instance IsGroupElement NewOrderSingleTradingSessionsGroupElem where
  type GroupNumField NewOrderSingleTradingSessionsGroupElem = NoTradingSessions
  mkGroupNum Proxy = NoTradingSessions
  countGroupNum Proxy = unNoTradingSessions

makeNewOrderSingleTradingSessionsGroupElem :: TradingSessionID -> NewOrderSingleTradingSessionsGroupElem
makeNewOrderSingleTradingSessionsGroupElem newOrderSingleTradingSessionsGroupElemTradingSessionID =
  let newOrderSingleTradingSessionsGroupElemTradingSessionSubID = Nothing
   in (NewOrderSingleTradingSessionsGroupElem {..})
