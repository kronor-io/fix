{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.RegulatoryTradeIDsGroupElem where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.MsgType
import FIX.Fields.NoRegulatoryTradeIDs
import FIX.Fields.RegulatoryTradeID
import FIX.Fields.RegulatoryTradeIDType
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "NoRegulatoryTradeIDs"
--   , groupNumberField = "NoRegulatoryTradeIDs"
--   , groupPieces =
--       [ MessagePieceField "RegulatoryTradeID" True
--       , MessagePieceField "RegulatoryTradeIDType" False
--       ]
--   }
data RegulatoryTradeIDsGroupElem = RegulatoryTradeIDsGroupElem
  { regulatoryTradeIDsGroupElemRegulatoryTradeID :: !RegulatoryTradeID,
    regulatoryTradeIDsGroupElemRegulatoryTradeIDType :: !(Maybe RegulatoryTradeIDType)
  }
  deriving stock (Show, Eq, Generic)

instance Validity RegulatoryTradeIDsGroupElem

instance IsComponent RegulatoryTradeIDsGroupElem where
  toComponentFields ((RegulatoryTradeIDsGroupElem {..})) =
    mconcat
      [ requiredFieldB regulatoryTradeIDsGroupElemRegulatoryTradeID,
        optionalFieldB regulatoryTradeIDsGroupElemRegulatoryTradeIDType
      ]
  fromComponentFields = do
    regulatoryTradeIDsGroupElemRegulatoryTradeID <- requiredFieldP
    regulatoryTradeIDsGroupElemRegulatoryTradeIDType <- optionalFieldP
    pure (RegulatoryTradeIDsGroupElem {..})

instance IsGroupElement RegulatoryTradeIDsGroupElem where
  type GroupNumField RegulatoryTradeIDsGroupElem = NoRegulatoryTradeIDs
  mkGroupNum Proxy = NoRegulatoryTradeIDs
  countGroupNum Proxy = unNoRegulatoryTradeIDs

makeRegulatoryTradeIDsGroupElem :: RegulatoryTradeID -> RegulatoryTradeIDsGroupElem
makeRegulatoryTradeIDsGroupElem regulatoryTradeIDsGroupElemRegulatoryTradeID =
  let regulatoryTradeIDsGroupElemRegulatoryTradeIDType = Nothing
   in (RegulatoryTradeIDsGroupElem {..})
