{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.ExecutionReportLegsGroupElem where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Components.LegExAnteData
import FIX.Fields.LegCFICode
import FIX.Fields.LegMaturityDate
import FIX.Fields.LegMidPx
import FIX.Fields.LegPrice
import FIX.Fields.LegRefID
import FIX.Fields.LegSecurityID
import FIX.Fields.LegSecurityIDSource
import FIX.Fields.LegSettlDate
import FIX.Fields.LegSide
import FIX.Fields.LegSymbol
import FIX.Fields.LegUPICode
import FIX.Fields.MsgType
import FIX.Fields.NoLegs
import FIX.Groups.Class
import FIX.Groups.ExecutionReportLegsLegAllocsGroupElem
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "ExecutionReportLegs"
--   , groupNumberField = "NoLegs"
--   , groupPieces =
--       [ MessagePieceField "LegSymbol" True
--       , MessagePieceField "LegSecurityID" False
--       , MessagePieceField "LegSecurityIDSource" False
--       , MessagePieceField "LegCFICode" False
--       , MessagePieceField "LegUPICode" False
--       , MessagePieceField "LegMaturityDate" False
--       , MessagePieceField "LegSide" False
--       , MessagePieceField "LegRefID" False
--       , MessagePieceField "LegPrice" False
--       , MessagePieceField "LegSettlDate" False
--       , MessagePieceField "LegMidPx" False
--       , MessagePieceComponent "LegExAnteData" True
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "ExecutionReportLegsLegAllocs"
--             , groupNumberField = "NoLegAllocs"
--             , groupPieces =
--                 [ MessagePieceField "LegAllocAccount" True
--                 , MessagePieceField "LegAllocQty" False
--                 ]
--             }
--           False
--       ]
--   }
data ExecutionReportLegsGroupElem = ExecutionReportLegsGroupElem
  { executionReportLegsGroupElemLegSymbol :: !LegSymbol,
    executionReportLegsGroupElemLegSecurityID :: !(Maybe LegSecurityID),
    executionReportLegsGroupElemLegSecurityIDSource :: !(Maybe LegSecurityIDSource),
    executionReportLegsGroupElemLegCFICode :: !(Maybe LegCFICode),
    executionReportLegsGroupElemLegUPICode :: !(Maybe LegUPICode),
    executionReportLegsGroupElemLegMaturityDate :: !(Maybe LegMaturityDate),
    executionReportLegsGroupElemLegSide :: !(Maybe LegSide),
    executionReportLegsGroupElemLegRefID :: !(Maybe LegRefID),
    executionReportLegsGroupElemLegPrice :: !(Maybe LegPrice),
    executionReportLegsGroupElemLegSettlDate :: !(Maybe LegSettlDate),
    executionReportLegsGroupElemLegMidPx :: !(Maybe LegMidPx),
    executionReportLegsGroupElemLegExAnteData :: !LegExAnteData,
    executionReportLegsGroupElemExecutionReportLegsLegAllocsGroup :: ![ExecutionReportLegsLegAllocsGroupElem]
  }
  deriving stock (Show, Eq, Generic)

instance Validity ExecutionReportLegsGroupElem

instance IsComponent ExecutionReportLegsGroupElem where
  toComponentFields ((ExecutionReportLegsGroupElem {..})) =
    mconcat
      [ requiredFieldB executionReportLegsGroupElemLegSymbol,
        optionalFieldB executionReportLegsGroupElemLegSecurityID,
        optionalFieldB executionReportLegsGroupElemLegSecurityIDSource,
        optionalFieldB executionReportLegsGroupElemLegCFICode,
        optionalFieldB executionReportLegsGroupElemLegUPICode,
        optionalFieldB executionReportLegsGroupElemLegMaturityDate,
        optionalFieldB executionReportLegsGroupElemLegSide,
        optionalFieldB executionReportLegsGroupElemLegRefID,
        optionalFieldB executionReportLegsGroupElemLegPrice,
        optionalFieldB executionReportLegsGroupElemLegSettlDate,
        optionalFieldB executionReportLegsGroupElemLegMidPx,
        requiredComponentB executionReportLegsGroupElemLegExAnteData,
        optionalGroupB executionReportLegsGroupElemExecutionReportLegsLegAllocsGroup
      ]
  fromComponentFields = do
    executionReportLegsGroupElemLegSymbol <- requiredFieldP
    executionReportLegsGroupElemLegSecurityID <- optionalFieldP
    executionReportLegsGroupElemLegSecurityIDSource <- optionalFieldP
    executionReportLegsGroupElemLegCFICode <- optionalFieldP
    executionReportLegsGroupElemLegUPICode <- optionalFieldP
    executionReportLegsGroupElemLegMaturityDate <- optionalFieldP
    executionReportLegsGroupElemLegSide <- optionalFieldP
    executionReportLegsGroupElemLegRefID <- optionalFieldP
    executionReportLegsGroupElemLegPrice <- optionalFieldP
    executionReportLegsGroupElemLegSettlDate <- optionalFieldP
    executionReportLegsGroupElemLegMidPx <- optionalFieldP
    executionReportLegsGroupElemLegExAnteData <- requiredComponentP
    executionReportLegsGroupElemExecutionReportLegsLegAllocsGroup <- optionalGroupP
    pure (ExecutionReportLegsGroupElem {..})

instance IsGroupElement ExecutionReportLegsGroupElem where
  type GroupNumField ExecutionReportLegsGroupElem = NoLegs
  mkGroupNum Proxy = NoLegs
  countGroupNum Proxy = unNoLegs

makeExecutionReportLegsGroupElem :: LegSymbol -> (LegExAnteData -> ExecutionReportLegsGroupElem)
makeExecutionReportLegsGroupElem executionReportLegsGroupElemLegSymbol executionReportLegsGroupElemLegExAnteData =
  let executionReportLegsGroupElemLegSecurityID = Nothing
      executionReportLegsGroupElemLegSecurityIDSource = Nothing
      executionReportLegsGroupElemLegCFICode = Nothing
      executionReportLegsGroupElemLegUPICode = Nothing
      executionReportLegsGroupElemLegMaturityDate = Nothing
      executionReportLegsGroupElemLegSide = Nothing
      executionReportLegsGroupElemLegRefID = Nothing
      executionReportLegsGroupElemLegPrice = Nothing
      executionReportLegsGroupElemLegSettlDate = Nothing
      executionReportLegsGroupElemLegMidPx = Nothing
      executionReportLegsGroupElemExecutionReportLegsLegAllocsGroup = []
   in (ExecutionReportLegsGroupElem {..})
