{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.RegulatoryTradeIDGroupElem where

import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.MsgType
import FIX.Fields.NoRegulatoryTradeID
import FIX.Fields.RegulatoryTradeID
import FIX.Fields.RegulatoryTradeIDType
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "NoRegulatoryTradeID"
--   , groupNumberField = "NoRegulatoryTradeID"
--   , groupPieces =
--       [ MessagePieceField "RegulatoryTradeID" True
--       , MessagePieceField "RegulatoryTradeIDType" False
--       ]
--   }
data RegulatoryTradeIDGroupElem = RegulatoryTradeIDGroupElem
  { regulatoryTradeIDGroupElemRegulatoryTradeID :: !RegulatoryTradeID,
    regulatoryTradeIDGroupElemRegulatoryTradeIDType :: !(Maybe RegulatoryTradeIDType)
  }
  deriving stock (Show, Eq, Generic)

instance Validity RegulatoryTradeIDGroupElem

instance IsComponent RegulatoryTradeIDGroupElem where
  toComponentFields ((RegulatoryTradeIDGroupElem {..})) =
    mconcat
      [ requiredFieldB regulatoryTradeIDGroupElemRegulatoryTradeID,
        optionalFieldB regulatoryTradeIDGroupElemRegulatoryTradeIDType
      ]
  fromComponentFields = do
    regulatoryTradeIDGroupElemRegulatoryTradeID <- requiredFieldP
    regulatoryTradeIDGroupElemRegulatoryTradeIDType <- optionalFieldP
    pure (RegulatoryTradeIDGroupElem {..})

instance IsGroupElement RegulatoryTradeIDGroupElem where
  type GroupNumField RegulatoryTradeIDGroupElem = NoRegulatoryTradeID
  mkGroupNum Proxy = NoRegulatoryTradeID
  countGroupNum Proxy = unNoRegulatoryTradeID

makeRegulatoryTradeIDGroupElem :: RegulatoryTradeID -> RegulatoryTradeIDGroupElem
makeRegulatoryTradeIDGroupElem regulatoryTradeIDGroupElemRegulatoryTradeID =
  let regulatoryTradeIDGroupElemRegulatoryTradeIDType = Nothing
   in (RegulatoryTradeIDGroupElem {..})