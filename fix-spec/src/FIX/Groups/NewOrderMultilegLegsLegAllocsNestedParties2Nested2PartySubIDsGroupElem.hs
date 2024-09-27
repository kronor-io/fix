{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.NewOrderMultilegLegsLegAllocsNestedParties2Nested2PartySubIDsGroupElem where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.MsgType
import FIX.Fields.Nested2PartySubID
import FIX.Fields.Nested2PartySubIDType
import FIX.Fields.NoNested2PartySubIDs
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName =
--       "NewOrderMultilegLegsLegAllocsNestedParties2Nested2PartySubIDs"
--   , groupNumberField = "NoNested2PartySubIDs"
--   , groupPieces =
--       [ MessagePieceField "Nested2PartySubID" True
--       , MessagePieceField "Nested2PartySubIDType" False
--       ]
--   }
data NewOrderMultilegLegsLegAllocsNestedParties2Nested2PartySubIDsGroupElem = NewOrderMultilegLegsLegAllocsNestedParties2Nested2PartySubIDsGroupElem
  { newOrderMultilegLegsLegAllocsNestedParties2Nested2PartySubIDsGroupElemNested2PartySubID :: !Nested2PartySubID,
    newOrderMultilegLegsLegAllocsNestedParties2Nested2PartySubIDsGroupElemNested2PartySubIDType :: !(Maybe Nested2PartySubIDType)
  }
  deriving stock (Show, Eq, Generic)

instance Validity NewOrderMultilegLegsLegAllocsNestedParties2Nested2PartySubIDsGroupElem

instance IsComponent NewOrderMultilegLegsLegAllocsNestedParties2Nested2PartySubIDsGroupElem where
  toComponentFields ((NewOrderMultilegLegsLegAllocsNestedParties2Nested2PartySubIDsGroupElem {..})) =
    mconcat
      [ requiredFieldB newOrderMultilegLegsLegAllocsNestedParties2Nested2PartySubIDsGroupElemNested2PartySubID,
        optionalFieldB newOrderMultilegLegsLegAllocsNestedParties2Nested2PartySubIDsGroupElemNested2PartySubIDType
      ]
  fromComponentFields = do
    newOrderMultilegLegsLegAllocsNestedParties2Nested2PartySubIDsGroupElemNested2PartySubID <- requiredFieldP
    newOrderMultilegLegsLegAllocsNestedParties2Nested2PartySubIDsGroupElemNested2PartySubIDType <- optionalFieldP
    pure (NewOrderMultilegLegsLegAllocsNestedParties2Nested2PartySubIDsGroupElem {..})

instance IsGroupElement NewOrderMultilegLegsLegAllocsNestedParties2Nested2PartySubIDsGroupElem where
  type GroupNumField NewOrderMultilegLegsLegAllocsNestedParties2Nested2PartySubIDsGroupElem = NoNested2PartySubIDs
  mkGroupNum Proxy = NoNested2PartySubIDs
  countGroupNum Proxy = unNoNested2PartySubIDs

makeNewOrderMultilegLegsLegAllocsNestedParties2Nested2PartySubIDsGroupElem :: Nested2PartySubID -> NewOrderMultilegLegsLegAllocsNestedParties2Nested2PartySubIDsGroupElem
makeNewOrderMultilegLegsLegAllocsNestedParties2Nested2PartySubIDsGroupElem newOrderMultilegLegsLegAllocsNestedParties2Nested2PartySubIDsGroupElemNested2PartySubID =
  let newOrderMultilegLegsLegAllocsNestedParties2Nested2PartySubIDsGroupElemNested2PartySubIDType = Nothing
   in (NewOrderMultilegLegsLegAllocsNestedParties2Nested2PartySubIDsGroupElem {..})
