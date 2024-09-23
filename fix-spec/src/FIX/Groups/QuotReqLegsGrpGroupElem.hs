{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.QuotReqLegsGrpGroupElem where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.LegMaturityDate
import FIX.Fields.LegQty
import FIX.Fields.LegRefID
import FIX.Fields.LegSettlDate
import FIX.Fields.LegSide
import FIX.Fields.LegSymbol
import FIX.Fields.MsgType
import FIX.Fields.NoLegs
import FIX.Groups.Class
import FIX.Groups.LegAllocsGroupElem
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "QuotReqLegsGrp"
--   , groupNumberField = "NoLegs"
--   , groupPieces =
--       [ MessagePieceField "LegSymbol" True
--       , MessagePieceField "LegMaturityDate" True
--       , MessagePieceField "LegSide" True
--       , MessagePieceField "LegQty" True
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "NoLegAllocs"
--             , groupNumberField = "NoLegAllocs"
--             , groupPieces =
--                 [ MessagePieceField "LegAllocAccount" True
--                 , MessagePieceField "LegAllocQty" True
--                 , MessagePieceGroup
--                     GroupSpec
--                       { groupName = "NoNestedPartyIDs"
--                       , groupNumberField = "NoNestedPartyIDs"
--                       , groupPieces =
--                           [ MessagePieceField "NestedPartyID" True
--                           , MessagePieceField "NestedPartyIDSource" False
--                           , MessagePieceField "NestedPartyRole" False
--                           , MessagePieceField "NestedPartyRoleQualifier" False
--                           ]
--                       }
--                     False
--                 ]
--             }
--           True
--       , MessagePieceField "LegRefID" True
--       , MessagePieceField "LegSettlDate" True
--       ]
--   }
data QuotReqLegsGrpGroupElem = QuotReqLegsGrpGroupElem
  { quotReqLegsGrpGroupElemLegSymbol :: !LegSymbol,
    quotReqLegsGrpGroupElemLegMaturityDate :: !LegMaturityDate,
    quotReqLegsGrpGroupElemLegSide :: !LegSide,
    quotReqLegsGrpGroupElemLegQty :: !LegQty,
    quotReqLegsGrpGroupElemLegAllocsGroup :: !(NonEmpty LegAllocsGroupElem),
    quotReqLegsGrpGroupElemLegRefID :: !LegRefID,
    quotReqLegsGrpGroupElemLegSettlDate :: !LegSettlDate
  }
  deriving stock (Show, Eq, Generic)

instance Validity QuotReqLegsGrpGroupElem

instance IsComponent QuotReqLegsGrpGroupElem where
  toComponentFields ((QuotReqLegsGrpGroupElem {..})) =
    mconcat
      [ requiredFieldB quotReqLegsGrpGroupElemLegSymbol,
        requiredFieldB quotReqLegsGrpGroupElemLegMaturityDate,
        requiredFieldB quotReqLegsGrpGroupElemLegSide,
        requiredFieldB quotReqLegsGrpGroupElemLegQty,
        requiredGroupB quotReqLegsGrpGroupElemLegAllocsGroup,
        requiredFieldB quotReqLegsGrpGroupElemLegRefID,
        requiredFieldB quotReqLegsGrpGroupElemLegSettlDate
      ]
  fromComponentFields = do
    quotReqLegsGrpGroupElemLegSymbol <- requiredFieldP
    quotReqLegsGrpGroupElemLegMaturityDate <- requiredFieldP
    quotReqLegsGrpGroupElemLegSide <- requiredFieldP
    quotReqLegsGrpGroupElemLegQty <- requiredFieldP
    quotReqLegsGrpGroupElemLegAllocsGroup <- requiredGroupP
    quotReqLegsGrpGroupElemLegRefID <- requiredFieldP
    quotReqLegsGrpGroupElemLegSettlDate <- requiredFieldP
    pure (QuotReqLegsGrpGroupElem {..})

instance IsGroupElement QuotReqLegsGrpGroupElem where
  type GroupNumField QuotReqLegsGrpGroupElem = NoLegs
  mkGroupNum Proxy = NoLegs
  countGroupNum Proxy = unNoLegs

makeQuotReqLegsGrpGroupElem :: LegSymbol -> (LegMaturityDate -> (LegSide -> (LegQty -> (NonEmpty LegAllocsGroupElem -> (LegRefID -> (LegSettlDate -> QuotReqLegsGrpGroupElem))))))
makeQuotReqLegsGrpGroupElem quotReqLegsGrpGroupElemLegSymbol quotReqLegsGrpGroupElemLegMaturityDate quotReqLegsGrpGroupElemLegSide quotReqLegsGrpGroupElemLegQty quotReqLegsGrpGroupElemLegAllocsGroup quotReqLegsGrpGroupElemLegRefID quotReqLegsGrpGroupElemLegSettlDate =
  let
   in (QuotReqLegsGrpGroupElem {..})
