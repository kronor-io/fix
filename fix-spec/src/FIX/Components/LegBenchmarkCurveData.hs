{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Components.LegBenchmarkCurveData where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.LegBenchmarkCurveCurrency
import FIX.Fields.LegBenchmarkCurveName
import FIX.Fields.LegBenchmarkCurvePoint
import FIX.Fields.LegBenchmarkPrice
import FIX.Fields.LegBenchmarkPriceType
import FIX.Fields.MsgType
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | ComponentSpec
--   { componentName = "LegBenchmarkCurveData"
--   , componentPieces =
--       [ MessagePieceField "LegBenchmarkCurveCurrency" False
--       , MessagePieceField "LegBenchmarkCurveName" False
--       , MessagePieceField "LegBenchmarkCurvePoint" False
--       , MessagePieceField "LegBenchmarkPrice" False
--       , MessagePieceField "LegBenchmarkPriceType" False
--       ]
--   }
data LegBenchmarkCurveData = LegBenchmarkCurveData
  { legBenchmarkCurveDataLegBenchmarkCurveCurrency :: !(Maybe LegBenchmarkCurveCurrency),
    legBenchmarkCurveDataLegBenchmarkCurveName :: !(Maybe LegBenchmarkCurveName),
    legBenchmarkCurveDataLegBenchmarkCurvePoint :: !(Maybe LegBenchmarkCurvePoint),
    legBenchmarkCurveDataLegBenchmarkPrice :: !(Maybe LegBenchmarkPrice),
    legBenchmarkCurveDataLegBenchmarkPriceType :: !(Maybe LegBenchmarkPriceType)
  }
  deriving stock (Show, Eq, Generic)

instance Validity LegBenchmarkCurveData

instance IsComponent LegBenchmarkCurveData where
  toComponentFields ((LegBenchmarkCurveData {..})) =
    mconcat
      [ optionalFieldB legBenchmarkCurveDataLegBenchmarkCurveCurrency,
        optionalFieldB legBenchmarkCurveDataLegBenchmarkCurveName,
        optionalFieldB legBenchmarkCurveDataLegBenchmarkCurvePoint,
        optionalFieldB legBenchmarkCurveDataLegBenchmarkPrice,
        optionalFieldB legBenchmarkCurveDataLegBenchmarkPriceType
      ]
  fromComponentFields = do
    legBenchmarkCurveDataLegBenchmarkCurveCurrency <- optionalFieldP
    legBenchmarkCurveDataLegBenchmarkCurveName <- optionalFieldP
    legBenchmarkCurveDataLegBenchmarkCurvePoint <- optionalFieldP
    legBenchmarkCurveDataLegBenchmarkPrice <- optionalFieldP
    legBenchmarkCurveDataLegBenchmarkPriceType <- optionalFieldP
    pure (LegBenchmarkCurveData {..})
