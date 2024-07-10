{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.QuotePartiesPartySubIDsGroupElem where

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
--   { groupName = "QuotePartiesPartySubIDs"
--   , groupNumberField = "NoPartySubIDs"
--   , groupPieces =
--       [ MessagePieceField "PartySubID" True
--       , MessagePieceField "PartySubIDType" False
--       ]
--   }
data QuotePartiesPartySubIDsGroupElem = QuotePartiesPartySubIDsGroupElem
  { quotePartiesPartySubIDsGroupElemPartySubID :: !PartySubID,
    quotePartiesPartySubIDsGroupElemPartySubIDType :: !(Maybe PartySubIDType)
  }
  deriving stock (Show, Eq, Generic)

instance Validity QuotePartiesPartySubIDsGroupElem

instance IsComponent QuotePartiesPartySubIDsGroupElem where
  toComponentFields ((QuotePartiesPartySubIDsGroupElem {..})) =
    mconcat
      [ requiredFieldB quotePartiesPartySubIDsGroupElemPartySubID,
        optionalFieldB quotePartiesPartySubIDsGroupElemPartySubIDType
      ]
  fromComponentFields = do
    quotePartiesPartySubIDsGroupElemPartySubID <- requiredFieldP
    quotePartiesPartySubIDsGroupElemPartySubIDType <- optionalFieldP
    pure (QuotePartiesPartySubIDsGroupElem {..})

instance IsGroupElement QuotePartiesPartySubIDsGroupElem where
  type GroupNumField QuotePartiesPartySubIDsGroupElem = NoPartySubIDs
  mkGroupNum Proxy = NoPartySubIDs
  countGroupNum Proxy = unNoPartySubIDs

makeQuotePartiesPartySubIDsGroupElem :: PartySubID -> QuotePartiesPartySubIDsGroupElem
makeQuotePartiesPartySubIDsGroupElem quotePartiesPartySubIDsGroupElemPartySubID =
  let quotePartiesPartySubIDsGroupElemPartySubIDType = Nothing
   in (QuotePartiesPartySubIDsGroupElem {..})
