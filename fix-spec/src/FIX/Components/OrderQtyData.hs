{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Components.OrderQtyData where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.CashOrderQty
import FIX.Fields.MsgType
import FIX.Fields.OrderPercent
import FIX.Fields.OrderQty
import FIX.Fields.RoundingDirection
import FIX.Fields.RoundingModulus
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | ComponentSpec
--   { componentName = "OrderQtyData"
--   , componentPieces =
--       [ MessagePieceField "OrderQty" True
--       , MessagePieceField "CashOrderQty" False
--       , MessagePieceField "OrderPercent" False
--       , MessagePieceField "RoundingDirection" False
--       , MessagePieceField "RoundingModulus" False
--       ]
--   }
data OrderQtyData = OrderQtyData
  { orderQtyDataOrderQty :: !OrderQty,
    orderQtyDataCashOrderQty :: !(Maybe CashOrderQty),
    orderQtyDataOrderPercent :: !(Maybe OrderPercent),
    orderQtyDataRoundingDirection :: !(Maybe RoundingDirection),
    orderQtyDataRoundingModulus :: !(Maybe RoundingModulus)
  }
  deriving stock (Show, Eq, Generic)

instance Validity OrderQtyData

instance IsComponent OrderQtyData where
  toComponentFields ((OrderQtyData {..})) =
    mconcat
      [ requiredFieldB orderQtyDataOrderQty,
        optionalFieldB orderQtyDataCashOrderQty,
        optionalFieldB orderQtyDataOrderPercent,
        optionalFieldB orderQtyDataRoundingDirection,
        optionalFieldB orderQtyDataRoundingModulus
      ]
  fromComponentFields = do
    orderQtyDataOrderQty <- requiredFieldP
    orderQtyDataCashOrderQty <- optionalFieldP
    orderQtyDataOrderPercent <- optionalFieldP
    orderQtyDataRoundingDirection <- optionalFieldP
    orderQtyDataRoundingModulus <- optionalFieldP
    pure (OrderQtyData {..})

makeOrderQtyData :: OrderQty -> OrderQtyData
makeOrderQtyData orderQtyDataOrderQty =
  let orderQtyDataCashOrderQty = Nothing
      orderQtyDataOrderPercent = Nothing
      orderQtyDataRoundingDirection = Nothing
      orderQtyDataRoundingModulus = Nothing
   in (OrderQtyData {..})
