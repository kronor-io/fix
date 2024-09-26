{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages.QuoteCancel where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.Account
import FIX.Fields.AccountType
import FIX.Fields.AcctIDSource
import FIX.Fields.MsgType
import FIX.Fields.ProductType
import FIX.Fields.QuoteCancelType
import FIX.Fields.QuoteID
import FIX.Fields.QuoteReqID
import FIX.Fields.QuoteResponseLevel
import FIX.Fields.TradingSessionID
import FIX.Fields.TradingSessionSubID
import FIX.Fields.Username
import FIX.Groups.Class
import FIX.Groups.QuoteCancelPartiesGroupElem
import FIX.Groups.QuoteCancelQuoteEntriesGroupElem
import FIX.Messages.Class
import GHC.Generics (Generic)

-- | MessageSpec
--   { messageName = "QuoteCancel"
--   , messageType = "Z"
--   , messageCategory = "app"
--   , messagePieces =
--       [ MessagePieceField "QuoteReqID" False
--       , MessagePieceField "QuoteID" True
--       , MessagePieceField "QuoteCancelType" True
--       , MessagePieceField "QuoteResponseLevel" False
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "QuoteCancelParties"
--             , groupNumberField = "NoPartyIDs"
--             , groupPieces =
--                 [ MessagePieceField "PartyID" True
--                 , MessagePieceField "PartyIDSource" False
--                 , MessagePieceField "PartyRole" False
--                 , MessagePieceField "PartyRoleQualifier" False
--                 , MessagePieceGroup
--                     GroupSpec
--                       { groupName = "QuoteCancelPartiesPartySubIDs"
--                       , groupNumberField = "NoPartySubIDs"
--                       , groupPieces =
--                           [ MessagePieceField "PartySubID" True
--                           , MessagePieceField "PartySubIDType" False
--                           ]
--                       }
--                     False
--                 ]
--             }
--           False
--       , MessagePieceField "Account" False
--       , MessagePieceField "Username" False
--       , MessagePieceField "ProductType" False
--       , MessagePieceField "AcctIDSource" False
--       , MessagePieceField "AccountType" False
--       , MessagePieceField "TradingSessionID" False
--       , MessagePieceField "TradingSessionSubID" False
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "QuoteCancelQuoteEntries"
--             , groupNumberField = "NoQuoteEntries"
--             , groupPieces =
--                 [ MessagePieceComponent "Instrument" True
--                 , MessagePieceComponent "FinancingDetails" True
--                 , MessagePieceGroup
--                     GroupSpec
--                       { groupName = "QuoteCancelQuoteEntriesUnderlyings"
--                       , groupNumberField = "NoUnderlyings"
--                       , groupPieces =
--                           [ MessagePieceComponent "UnderlyingInstrument" True ]
--                       }
--                     False
--                 , MessagePieceGroup
--                     GroupSpec
--                       { groupName = "QuoteCancelQuoteEntriesLegs"
--                       , groupNumberField = "NoLegs"
--                       , groupPieces = [ MessagePieceComponent "InstrumentLeg" True ]
--                       }
--                     False
--                 ]
--             }
--           False
--       ]
--   }
data QuoteCancel = QuoteCancel
  { quoteCancelQuoteReqID :: !(Maybe QuoteReqID),
    quoteCancelQuoteID :: !QuoteID,
    quoteCancelQuoteCancelType :: !QuoteCancelType,
    quoteCancelQuoteResponseLevel :: !(Maybe QuoteResponseLevel),
    quoteCancelQuoteCancelPartiesGroup :: ![QuoteCancelPartiesGroupElem],
    quoteCancelAccount :: !(Maybe Account),
    quoteCancelUsername :: !(Maybe Username),
    quoteCancelProductType :: !(Maybe ProductType),
    quoteCancelAcctIDSource :: !(Maybe AcctIDSource),
    quoteCancelAccountType :: !(Maybe AccountType),
    quoteCancelTradingSessionID :: !(Maybe TradingSessionID),
    quoteCancelTradingSessionSubID :: !(Maybe TradingSessionSubID),
    quoteCancelQuoteCancelQuoteEntriesGroup :: ![QuoteCancelQuoteEntriesGroupElem]
  }
  deriving stock (Show, Eq, Generic)

instance Validity QuoteCancel

instance IsComponent QuoteCancel where
  toComponentFields ((QuoteCancel {..})) =
    mconcat
      [ optionalFieldB quoteCancelQuoteReqID,
        requiredFieldB quoteCancelQuoteID,
        requiredFieldB quoteCancelQuoteCancelType,
        optionalFieldB quoteCancelQuoteResponseLevel,
        optionalGroupB quoteCancelQuoteCancelPartiesGroup,
        optionalFieldB quoteCancelAccount,
        optionalFieldB quoteCancelUsername,
        optionalFieldB quoteCancelProductType,
        optionalFieldB quoteCancelAcctIDSource,
        optionalFieldB quoteCancelAccountType,
        optionalFieldB quoteCancelTradingSessionID,
        optionalFieldB quoteCancelTradingSessionSubID,
        optionalGroupB quoteCancelQuoteCancelQuoteEntriesGroup
      ]
  fromComponentFields = do
    quoteCancelQuoteReqID <- optionalFieldP
    quoteCancelQuoteID <- requiredFieldP
    quoteCancelQuoteCancelType <- requiredFieldP
    quoteCancelQuoteResponseLevel <- optionalFieldP
    quoteCancelQuoteCancelPartiesGroup <- optionalGroupP
    quoteCancelAccount <- optionalFieldP
    quoteCancelUsername <- optionalFieldP
    quoteCancelProductType <- optionalFieldP
    quoteCancelAcctIDSource <- optionalFieldP
    quoteCancelAccountType <- optionalFieldP
    quoteCancelTradingSessionID <- optionalFieldP
    quoteCancelTradingSessionSubID <- optionalFieldP
    quoteCancelQuoteCancelQuoteEntriesGroup <- optionalGroupP
    pure (QuoteCancel {..})

instance IsMessage QuoteCancel where
  messageType Proxy = MsgTypeQuoteCancel

makeQuoteCancel :: QuoteID -> (QuoteCancelType -> QuoteCancel)
makeQuoteCancel quoteCancelQuoteID quoteCancelQuoteCancelType =
  let quoteCancelQuoteReqID = Nothing
      quoteCancelQuoteResponseLevel = Nothing
      quoteCancelQuoteCancelPartiesGroup = []
      quoteCancelAccount = Nothing
      quoteCancelUsername = Nothing
      quoteCancelProductType = Nothing
      quoteCancelAcctIDSource = Nothing
      quoteCancelAccountType = Nothing
      quoteCancelTradingSessionID = Nothing
      quoteCancelTradingSessionSubID = Nothing
      quoteCancelQuoteCancelQuoteEntriesGroup = []
   in (QuoteCancel {..})
