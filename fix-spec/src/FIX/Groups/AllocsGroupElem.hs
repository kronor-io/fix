{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.AllocsGroupElem where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.AllocAccount
import FIX.Fields.AllocQty
import FIX.Fields.MsgType
import FIX.Fields.NoAllocs
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "NoAllocs"
--   , groupNumberField = "NoAllocs"
--   , groupPieces =
--       [ MessagePieceField "AllocAccount" True
--       , MessagePieceField "AllocQty" True
--       ]
--   }
data AllocsGroupElem = AllocsGroupElem
  { allocsGroupElemAllocAccount :: !AllocAccount,
    allocsGroupElemAllocQty :: !AllocQty
  }
  deriving stock (Show, Eq, Generic)

instance Validity AllocsGroupElem

instance IsComponent AllocsGroupElem where
  toComponentFields ((AllocsGroupElem {..})) =
    mconcat
      [ requiredFieldB allocsGroupElemAllocAccount,
        requiredFieldB allocsGroupElemAllocQty
      ]
  fromComponentFields = do
    allocsGroupElemAllocAccount <- requiredFieldP
    allocsGroupElemAllocQty <- requiredFieldP
    pure (AllocsGroupElem {..})

instance IsGroupElement AllocsGroupElem where
  type GroupNumField AllocsGroupElem = NoAllocs
  mkGroupNum Proxy = NoAllocs
  countGroupNum Proxy = unNoAllocs

makeAllocsGroupElem :: AllocAccount -> (AllocQty -> AllocsGroupElem)
makeAllocsGroupElem allocsGroupElemAllocAccount allocsGroupElemAllocQty =
  let
   in (AllocsGroupElem {..})
