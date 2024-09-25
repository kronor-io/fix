{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages.SecurityDefinitionRequest where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Components.Instrument
import FIX.Components.InstrumentExtension
import FIX.Fields.Currency
import FIX.Fields.EncodedText
import FIX.Fields.ExpirationCycle
import FIX.Fields.MsgType
import FIX.Fields.SecurityReqID
import FIX.Fields.SecurityRequestType
import FIX.Fields.SubscriptionRequestType
import FIX.Fields.Text
import FIX.Fields.TradingSessionID
import FIX.Fields.TradingSessionSubID
import FIX.Groups.Class
import FIX.Groups.LegsGroupElem
import FIX.Groups.UnderlyingsGroupElem
import FIX.Messages.Class
import GHC.Generics (Generic)

-- | MessageSpec
--   { messageName = "SecurityDefinitionRequest"
--   , messageType = "c"
--   , messageCategory = "app"
--   , messagePieces =
--       [ MessagePieceField "SecurityReqID" True
--       , MessagePieceField "SecurityRequestType" True
--       , MessagePieceComponent "Instrument" False
--       , MessagePieceComponent "InstrumentExtension" True
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "NoUnderlyings"
--             , groupNumberField = "NoUnderlyings"
--             , groupPieces =
--                 [ MessagePieceComponent "UnderlyingInstrument" True ]
--             }
--           False
--       , MessagePieceField "Currency" False
--       , MessagePieceField "Text" False
--       , MessagePieceField "EncodedText" False
--       , MessagePieceField "TradingSessionID" False
--       , MessagePieceField "TradingSessionSubID" False
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "NoLegs"
--             , groupNumberField = "NoLegs"
--             , groupPieces = [ MessagePieceComponent "InstrumentLeg" True ]
--             }
--           False
--       , MessagePieceField "ExpirationCycle" False
--       , MessagePieceField "SubscriptionRequestType" False
--       ]
--   }
data SecurityDefinitionRequest = SecurityDefinitionRequest
  { securityDefinitionRequestSecurityReqID :: !SecurityReqID,
    securityDefinitionRequestSecurityRequestType :: !SecurityRequestType,
    securityDefinitionRequestInstrument :: !(Maybe Instrument),
    securityDefinitionRequestInstrumentExtension :: !InstrumentExtension,
    securityDefinitionRequestUnderlyingsGroup :: ![UnderlyingsGroupElem],
    securityDefinitionRequestCurrency :: !(Maybe Currency),
    securityDefinitionRequestText :: !(Maybe Text),
    securityDefinitionRequestEncodedText :: !(Maybe EncodedText),
    securityDefinitionRequestTradingSessionID :: !(Maybe TradingSessionID),
    securityDefinitionRequestTradingSessionSubID :: !(Maybe TradingSessionSubID),
    securityDefinitionRequestLegsGroup :: ![LegsGroupElem],
    securityDefinitionRequestExpirationCycle :: !(Maybe ExpirationCycle),
    securityDefinitionRequestSubscriptionRequestType :: !(Maybe SubscriptionRequestType)
  }
  deriving stock (Show, Eq, Generic)

instance Validity SecurityDefinitionRequest

instance IsComponent SecurityDefinitionRequest where
  toComponentFields ((SecurityDefinitionRequest {..})) =
    mconcat
      [ requiredFieldB securityDefinitionRequestSecurityReqID,
        requiredFieldB securityDefinitionRequestSecurityRequestType,
        optionalComponentB securityDefinitionRequestInstrument,
        requiredComponentB securityDefinitionRequestInstrumentExtension,
        optionalGroupB securityDefinitionRequestUnderlyingsGroup,
        optionalFieldB securityDefinitionRequestCurrency,
        optionalFieldB securityDefinitionRequestText,
        optionalFieldB securityDefinitionRequestEncodedText,
        optionalFieldB securityDefinitionRequestTradingSessionID,
        optionalFieldB securityDefinitionRequestTradingSessionSubID,
        optionalGroupB securityDefinitionRequestLegsGroup,
        optionalFieldB securityDefinitionRequestExpirationCycle,
        optionalFieldB securityDefinitionRequestSubscriptionRequestType
      ]
  fromComponentFields = do
    securityDefinitionRequestSecurityReqID <- requiredFieldP
    securityDefinitionRequestSecurityRequestType <- requiredFieldP
    securityDefinitionRequestInstrument <- optionalComponentP
    securityDefinitionRequestInstrumentExtension <- requiredComponentP
    securityDefinitionRequestUnderlyingsGroup <- optionalGroupP
    securityDefinitionRequestCurrency <- optionalFieldP
    securityDefinitionRequestText <- optionalFieldP
    securityDefinitionRequestEncodedText <- optionalFieldP
    securityDefinitionRequestTradingSessionID <- optionalFieldP
    securityDefinitionRequestTradingSessionSubID <- optionalFieldP
    securityDefinitionRequestLegsGroup <- optionalGroupP
    securityDefinitionRequestExpirationCycle <- optionalFieldP
    securityDefinitionRequestSubscriptionRequestType <- optionalFieldP
    pure (SecurityDefinitionRequest {..})

instance IsMessage SecurityDefinitionRequest where
  messageType Proxy = MsgTypeSecurityDefinitionRequest

makeSecurityDefinitionRequest :: SecurityReqID -> (SecurityRequestType -> (InstrumentExtension -> SecurityDefinitionRequest))
makeSecurityDefinitionRequest securityDefinitionRequestSecurityReqID securityDefinitionRequestSecurityRequestType securityDefinitionRequestInstrumentExtension =
  let securityDefinitionRequestInstrument = Nothing
      securityDefinitionRequestUnderlyingsGroup = []
      securityDefinitionRequestCurrency = Nothing
      securityDefinitionRequestText = Nothing
      securityDefinitionRequestEncodedText = Nothing
      securityDefinitionRequestTradingSessionID = Nothing
      securityDefinitionRequestTradingSessionSubID = Nothing
      securityDefinitionRequestLegsGroup = []
      securityDefinitionRequestExpirationCycle = Nothing
      securityDefinitionRequestSubscriptionRequestType = Nothing
   in (SecurityDefinitionRequest {..})
