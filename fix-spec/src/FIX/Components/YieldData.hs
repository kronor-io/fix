{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Components.YieldData where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.MsgType
import FIX.Fields.Yield
import FIX.Fields.YieldCalcDate
import FIX.Fields.YieldRedemptionDate
import FIX.Fields.YieldRedemptionPrice
import FIX.Fields.YieldRedemptionPriceType
import FIX.Fields.YieldType
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | ComponentSpec
--   { componentName = "YieldData"
--   , componentPieces =
--       [ MessagePieceField "YieldType" False
--       , MessagePieceField "Yield" False
--       , MessagePieceField "YieldCalcDate" False
--       , MessagePieceField "YieldRedemptionDate" False
--       , MessagePieceField "YieldRedemptionPrice" False
--       , MessagePieceField "YieldRedemptionPriceType" False
--       ]
--   }
data YieldData = YieldData
  { yieldDataYieldType :: !(Maybe YieldType),
    yieldDataYield :: !(Maybe Yield),
    yieldDataYieldCalcDate :: !(Maybe YieldCalcDate),
    yieldDataYieldRedemptionDate :: !(Maybe YieldRedemptionDate),
    yieldDataYieldRedemptionPrice :: !(Maybe YieldRedemptionPrice),
    yieldDataYieldRedemptionPriceType :: !(Maybe YieldRedemptionPriceType)
  }
  deriving stock (Show, Eq, Generic)

instance Validity YieldData

instance IsComponent YieldData where
  toComponentFields ((YieldData {..})) =
    mconcat
      [ optionalFieldB yieldDataYieldType,
        optionalFieldB yieldDataYield,
        optionalFieldB yieldDataYieldCalcDate,
        optionalFieldB yieldDataYieldRedemptionDate,
        optionalFieldB yieldDataYieldRedemptionPrice,
        optionalFieldB yieldDataYieldRedemptionPriceType
      ]
  fromComponentFields = do
    yieldDataYieldType <- optionalFieldP
    yieldDataYield <- optionalFieldP
    yieldDataYieldCalcDate <- optionalFieldP
    yieldDataYieldRedemptionDate <- optionalFieldP
    yieldDataYieldRedemptionPrice <- optionalFieldP
    yieldDataYieldRedemptionPriceType <- optionalFieldP
    pure (YieldData {..})

makeYieldData :: YieldData
makeYieldData =
  let yieldDataYieldType = Nothing
      yieldDataYield = Nothing
      yieldDataYieldCalcDate = Nothing
      yieldDataYieldRedemptionDate = Nothing
      yieldDataYieldRedemptionPrice = Nothing
      yieldDataYieldRedemptionPriceType = Nothing
   in (YieldData {..})
