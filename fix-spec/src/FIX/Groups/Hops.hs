{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.Hops where

import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.HopCompID
import FIX.Fields.HopRefID
import FIX.Fields.HopSendingTime
import FIX.Fields.MsgType
import FIX.Fields.NoHops
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "NoHops"
--   , groupPieces =
--       [ MessagePieceField "HopCompID" True
--       , MessagePieceField "HopSendingTime" False
--       , MessagePieceField "HopRefID" False
--       ]
--   }
data Hops = Hops
  { hopsHopCompID :: !HopCompID,
    hopsHopSendingTime :: !(Maybe HopSendingTime),
    hopsHopRefID :: !(Maybe HopRefID)
  }
  deriving stock (Show, Eq, Generic)

instance Validity Hops

instance IsComponent Hops where
  toComponentFields ((Hops {..})) =
    mconcat
      [ requiredFieldB hopsHopCompID,
        optionalFieldB hopsHopSendingTime,
        optionalFieldB hopsHopRefID
      ]
  fromComponentFields = do
    hopsHopCompID <- requiredFieldP
    hopsHopSendingTime <- optionalFieldP
    hopsHopRefID <- optionalFieldP
    pure (Hops {..})

instance IsGroupElement Hops where
  type GroupNumField Hops = NoHops
  mkGroupNum Proxy = NoHops
  countGroupNum Proxy = unNoHops
