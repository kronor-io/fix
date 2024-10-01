{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.NewOrderSingleAllocsNestedPartiesNestedPartySubIDsGroupElem where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.MsgType
import FIX.Fields.NestedPartySubID
import FIX.Fields.NestedPartySubIDType
import FIX.Fields.NoNestedPartySubIDs
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "NewOrderSingleAllocsNestedPartiesNestedPartySubIDs"
--   , groupNumberField = "NoNestedPartySubIDs"
--   , groupPieces =
--       [ MessagePieceField "NestedPartySubID" True
--       , MessagePieceField "NestedPartySubIDType" False
--       ]
--   }
data NewOrderSingleAllocsNestedPartiesNestedPartySubIDsGroupElem = NewOrderSingleAllocsNestedPartiesNestedPartySubIDsGroupElem
  { newOrderSingleAllocsNestedPartiesNestedPartySubIDsGroupElemNestedPartySubID :: !NestedPartySubID,
    newOrderSingleAllocsNestedPartiesNestedPartySubIDsGroupElemNestedPartySubIDType :: !(Maybe NestedPartySubIDType)
  }
  deriving stock (Show, Eq, Generic)

instance Validity NewOrderSingleAllocsNestedPartiesNestedPartySubIDsGroupElem

instance IsComponent NewOrderSingleAllocsNestedPartiesNestedPartySubIDsGroupElem where
  toComponentFields ((NewOrderSingleAllocsNestedPartiesNestedPartySubIDsGroupElem {..})) =
    mconcat
      [ requiredFieldB newOrderSingleAllocsNestedPartiesNestedPartySubIDsGroupElemNestedPartySubID,
        optionalFieldB newOrderSingleAllocsNestedPartiesNestedPartySubIDsGroupElemNestedPartySubIDType
      ]
  fromComponentFields = do
    newOrderSingleAllocsNestedPartiesNestedPartySubIDsGroupElemNestedPartySubID <- requiredFieldP
    newOrderSingleAllocsNestedPartiesNestedPartySubIDsGroupElemNestedPartySubIDType <- optionalFieldP
    pure (NewOrderSingleAllocsNestedPartiesNestedPartySubIDsGroupElem {..})

instance IsGroupElement NewOrderSingleAllocsNestedPartiesNestedPartySubIDsGroupElem where
  type GroupNumField NewOrderSingleAllocsNestedPartiesNestedPartySubIDsGroupElem = NoNestedPartySubIDs
  mkGroupNum Proxy = NoNestedPartySubIDs
  countGroupNum Proxy = unNoNestedPartySubIDs

makeNewOrderSingleAllocsNestedPartiesNestedPartySubIDsGroupElem :: NestedPartySubID -> NewOrderSingleAllocsNestedPartiesNestedPartySubIDsGroupElem
makeNewOrderSingleAllocsNestedPartiesNestedPartySubIDsGroupElem newOrderSingleAllocsNestedPartiesNestedPartySubIDsGroupElemNestedPartySubID =
  let newOrderSingleAllocsNestedPartiesNestedPartySubIDsGroupElemNestedPartySubIDType = Nothing
   in (NewOrderSingleAllocsNestedPartiesNestedPartySubIDsGroupElem {..})
