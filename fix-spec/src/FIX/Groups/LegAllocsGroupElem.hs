{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.LegAllocsGroupElem where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Components.NestedParties
import FIX.Fields.LegAllocAccount
import FIX.Fields.LegAllocQty
import FIX.Fields.MsgType
import FIX.Fields.NoLegAllocs
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "NoLegAllocs"
--   , groupNumberField = "NoLegAllocs"
--   , groupPieces =
--       [ MessagePieceField "LegAllocAccount" True
--       , MessagePieceField "LegAllocQty" True
--       , MessagePieceComponent "NestedParties" True
--       ]
--   }
data LegAllocsGroupElem = LegAllocsGroupElem
  { legAllocsGroupElemLegAllocAccount :: !LegAllocAccount,
    legAllocsGroupElemLegAllocQty :: !LegAllocQty,
    legAllocsGroupElemNestedParties :: !NestedParties
  }
  deriving stock (Show, Eq, Generic)

instance Validity LegAllocsGroupElem

instance IsComponent LegAllocsGroupElem where
  toComponentFields ((LegAllocsGroupElem {..})) =
    mconcat
      [ requiredFieldB legAllocsGroupElemLegAllocAccount,
        requiredFieldB legAllocsGroupElemLegAllocQty,
        requiredComponentB legAllocsGroupElemNestedParties
      ]
  fromComponentFields = do
    legAllocsGroupElemLegAllocAccount <- requiredFieldP
    legAllocsGroupElemLegAllocQty <- requiredFieldP
    legAllocsGroupElemNestedParties <- requiredComponentP
    pure (LegAllocsGroupElem {..})

instance IsGroupElement LegAllocsGroupElem where
  type GroupNumField LegAllocsGroupElem = NoLegAllocs
  mkGroupNum Proxy = NoLegAllocs
  countGroupNum Proxy = unNoLegAllocs

makeLegAllocsGroupElem :: LegAllocAccount -> (LegAllocQty -> (NestedParties -> LegAllocsGroupElem))
makeLegAllocsGroupElem legAllocsGroupElemLegAllocAccount legAllocsGroupElemLegAllocQty legAllocsGroupElemNestedParties =
  let
   in (LegAllocsGroupElem {..})
