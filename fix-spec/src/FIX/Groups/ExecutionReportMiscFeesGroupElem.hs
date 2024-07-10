{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.ExecutionReportMiscFeesGroupElem where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.MiscFeeAmt
import FIX.Fields.MiscFeeBasis
import FIX.Fields.MiscFeeCurr
import FIX.Fields.MiscFeeType
import FIX.Fields.MsgType
import FIX.Fields.NoMiscFees
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "ExecutionReportMiscFees"
--   , groupNumberField = "NoMiscFees"
--   , groupPieces =
--       [ MessagePieceField "MiscFeeAmt" True
--       , MessagePieceField "MiscFeeCurr" False
--       , MessagePieceField "MiscFeeType" False
--       , MessagePieceField "MiscFeeBasis" False
--       ]
--   }
data ExecutionReportMiscFeesGroupElem = ExecutionReportMiscFeesGroupElem
  { executionReportMiscFeesGroupElemMiscFeeAmt :: !MiscFeeAmt,
    executionReportMiscFeesGroupElemMiscFeeCurr :: !(Maybe MiscFeeCurr),
    executionReportMiscFeesGroupElemMiscFeeType :: !(Maybe MiscFeeType),
    executionReportMiscFeesGroupElemMiscFeeBasis :: !(Maybe MiscFeeBasis)
  }
  deriving stock (Show, Eq, Generic)

instance Validity ExecutionReportMiscFeesGroupElem

instance IsComponent ExecutionReportMiscFeesGroupElem where
  toComponentFields ((ExecutionReportMiscFeesGroupElem {..})) =
    mconcat
      [ requiredFieldB executionReportMiscFeesGroupElemMiscFeeAmt,
        optionalFieldB executionReportMiscFeesGroupElemMiscFeeCurr,
        optionalFieldB executionReportMiscFeesGroupElemMiscFeeType,
        optionalFieldB executionReportMiscFeesGroupElemMiscFeeBasis
      ]
  fromComponentFields = do
    executionReportMiscFeesGroupElemMiscFeeAmt <- requiredFieldP
    executionReportMiscFeesGroupElemMiscFeeCurr <- optionalFieldP
    executionReportMiscFeesGroupElemMiscFeeType <- optionalFieldP
    executionReportMiscFeesGroupElemMiscFeeBasis <- optionalFieldP
    pure (ExecutionReportMiscFeesGroupElem {..})

instance IsGroupElement ExecutionReportMiscFeesGroupElem where
  type GroupNumField ExecutionReportMiscFeesGroupElem = NoMiscFees
  mkGroupNum Proxy = NoMiscFees
  countGroupNum Proxy = unNoMiscFees

makeExecutionReportMiscFeesGroupElem :: MiscFeeAmt -> ExecutionReportMiscFeesGroupElem
makeExecutionReportMiscFeesGroupElem executionReportMiscFeesGroupElemMiscFeeAmt =
  let executionReportMiscFeesGroupElemMiscFeeCurr = Nothing
      executionReportMiscFeesGroupElemMiscFeeType = Nothing
      executionReportMiscFeesGroupElemMiscFeeBasis = Nothing
   in (ExecutionReportMiscFeesGroupElem {..})
