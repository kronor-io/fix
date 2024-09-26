{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.QuoteCancelPartiesPartySubIDsGroupElem where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.MsgType
import FIX.Fields.NoPartySubIDs
import FIX.Fields.PartySubID
import FIX.Fields.PartySubIDType
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "QuoteCancelPartiesPartySubIDs"
--   , groupNumberField = "NoPartySubIDs"
--   , groupPieces =
--       [ MessagePieceField "PartySubID" True
--       , MessagePieceField "PartySubIDType" False
--       ]
--   }
data QuoteCancelPartiesPartySubIDsGroupElem = QuoteCancelPartiesPartySubIDsGroupElem
  { quoteCancelPartiesPartySubIDsGroupElemPartySubID :: !PartySubID,
    quoteCancelPartiesPartySubIDsGroupElemPartySubIDType :: !(Maybe PartySubIDType)
  }
  deriving stock (Show, Eq, Generic)

instance Validity QuoteCancelPartiesPartySubIDsGroupElem

instance IsComponent QuoteCancelPartiesPartySubIDsGroupElem where
  toComponentFields ((QuoteCancelPartiesPartySubIDsGroupElem {..})) =
    mconcat
      [ requiredFieldB quoteCancelPartiesPartySubIDsGroupElemPartySubID,
        optionalFieldB quoteCancelPartiesPartySubIDsGroupElemPartySubIDType
      ]
  fromComponentFields = do
    quoteCancelPartiesPartySubIDsGroupElemPartySubID <- requiredFieldP
    quoteCancelPartiesPartySubIDsGroupElemPartySubIDType <- optionalFieldP
    pure (QuoteCancelPartiesPartySubIDsGroupElem {..})

instance IsGroupElement QuoteCancelPartiesPartySubIDsGroupElem where
  type GroupNumField QuoteCancelPartiesPartySubIDsGroupElem = NoPartySubIDs
  mkGroupNum Proxy = NoPartySubIDs
  countGroupNum Proxy = unNoPartySubIDs

makeQuoteCancelPartiesPartySubIDsGroupElem :: PartySubID -> QuoteCancelPartiesPartySubIDsGroupElem
makeQuoteCancelPartiesPartySubIDsGroupElem quoteCancelPartiesPartySubIDsGroupElemPartySubID =
  let quoteCancelPartiesPartySubIDsGroupElemPartySubIDType = Nothing
   in (QuoteCancelPartiesPartySubIDsGroupElem {..})
