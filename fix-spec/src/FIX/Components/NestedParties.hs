{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Components.NestedParties where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.MsgType
import FIX.Groups.Class
import FIX.Groups.NestedPartyIDsGroupElem
import GHC.Generics (Generic)

-- | ComponentSpec
--   { componentName = "NestedParties"
--   , componentPieces =
--       [ MessagePieceGroup
--           GroupSpec
--             { groupName = "NoNestedPartyIDs"
--             , groupPieces =
--                 [ MessagePieceField "NestedPartyID" True
--                 , MessagePieceField "NestedPartyIDSource" False
--                 , MessagePieceField "NestedPartyRole" False
--                 , MessagePieceComponent "NstdPtysSubGrp" False
--                 ]
--             }
--           False
--       ]
--   }
data NestedParties = NestedParties {nestedPartiesNestedPartyIDsGroup :: ![NestedPartyIDsGroupElem]}
  deriving stock (Show, Eq, Generic)

instance Validity NestedParties

instance IsComponent NestedParties where
  toComponentFields ((NestedParties {..})) = mconcat [optionalGroupB nestedPartiesNestedPartyIDsGroup]
  fromComponentFields = do
    nestedPartiesNestedPartyIDsGroup <- optionalGroupP
    pure (NestedParties {..})