{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Components.SpreadOrBenchmarkCurveData where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.BenchmarkCurveCurrency
import FIX.Fields.BenchmarkCurveName
import FIX.Fields.BenchmarkCurvePoint
import FIX.Fields.BenchmarkPrice
import FIX.Fields.BenchmarkPriceType
import FIX.Fields.BenchmarkSecurityID
import FIX.Fields.BenchmarkSecurityIDSource
import FIX.Fields.MsgType
import FIX.Fields.Spread
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | ComponentSpec
--   { componentName = "SpreadOrBenchmarkCurveData"
--   , componentPieces =
--       [ MessagePieceField "Spread" False
--       , MessagePieceField "BenchmarkCurveCurrency" False
--       , MessagePieceField "BenchmarkCurveName" False
--       , MessagePieceField "BenchmarkCurvePoint" False
--       , MessagePieceField "BenchmarkPrice" False
--       , MessagePieceField "BenchmarkPriceType" False
--       , MessagePieceField "BenchmarkSecurityID" False
--       , MessagePieceField "BenchmarkSecurityIDSource" False
--       ]
--   }
data SpreadOrBenchmarkCurveData = SpreadOrBenchmarkCurveData
  { spreadOrBenchmarkCurveDataSpread :: !(Maybe Spread),
    spreadOrBenchmarkCurveDataBenchmarkCurveCurrency :: !(Maybe BenchmarkCurveCurrency),
    spreadOrBenchmarkCurveDataBenchmarkCurveName :: !(Maybe BenchmarkCurveName),
    spreadOrBenchmarkCurveDataBenchmarkCurvePoint :: !(Maybe BenchmarkCurvePoint),
    spreadOrBenchmarkCurveDataBenchmarkPrice :: !(Maybe BenchmarkPrice),
    spreadOrBenchmarkCurveDataBenchmarkPriceType :: !(Maybe BenchmarkPriceType),
    spreadOrBenchmarkCurveDataBenchmarkSecurityID :: !(Maybe BenchmarkSecurityID),
    spreadOrBenchmarkCurveDataBenchmarkSecurityIDSource :: !(Maybe BenchmarkSecurityIDSource)
  }
  deriving stock (Show, Eq, Generic)

instance Validity SpreadOrBenchmarkCurveData

instance IsComponent SpreadOrBenchmarkCurveData where
  toComponentFields ((SpreadOrBenchmarkCurveData {..})) =
    mconcat
      [ optionalFieldB spreadOrBenchmarkCurveDataSpread,
        optionalFieldB spreadOrBenchmarkCurveDataBenchmarkCurveCurrency,
        optionalFieldB spreadOrBenchmarkCurveDataBenchmarkCurveName,
        optionalFieldB spreadOrBenchmarkCurveDataBenchmarkCurvePoint,
        optionalFieldB spreadOrBenchmarkCurveDataBenchmarkPrice,
        optionalFieldB spreadOrBenchmarkCurveDataBenchmarkPriceType,
        optionalFieldB spreadOrBenchmarkCurveDataBenchmarkSecurityID,
        optionalFieldB spreadOrBenchmarkCurveDataBenchmarkSecurityIDSource
      ]
  fromComponentFields = do
    spreadOrBenchmarkCurveDataSpread <- optionalFieldP
    spreadOrBenchmarkCurveDataBenchmarkCurveCurrency <- optionalFieldP
    spreadOrBenchmarkCurveDataBenchmarkCurveName <- optionalFieldP
    spreadOrBenchmarkCurveDataBenchmarkCurvePoint <- optionalFieldP
    spreadOrBenchmarkCurveDataBenchmarkPrice <- optionalFieldP
    spreadOrBenchmarkCurveDataBenchmarkPriceType <- optionalFieldP
    spreadOrBenchmarkCurveDataBenchmarkSecurityID <- optionalFieldP
    spreadOrBenchmarkCurveDataBenchmarkSecurityIDSource <- optionalFieldP
    pure (SpreadOrBenchmarkCurveData {..})
