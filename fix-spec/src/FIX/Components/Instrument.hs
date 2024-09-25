{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Components.Instrument where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.CFICode
import FIX.Fields.CPProgram
import FIX.Fields.CPRegType
import FIX.Fields.ContractMultiplier
import FIX.Fields.ContractSettlMonth
import FIX.Fields.CountryOfIssue
import FIX.Fields.CouponPaymentDate
import FIX.Fields.CouponRate
import FIX.Fields.CreditRating
import FIX.Fields.DatedDate
import FIX.Fields.EncodedIssuer
import FIX.Fields.EncodedSecurityDesc
import FIX.Fields.Factor
import FIX.Fields.InstrRegistry
import FIX.Fields.InterestAccrualDate
import FIX.Fields.IssueDate
import FIX.Fields.Issuer
import FIX.Fields.LocaleOfIssue
import FIX.Fields.MaturityDate
import FIX.Fields.MaturityDate2
import FIX.Fields.MaturityMonthYear
import FIX.Fields.MsgType
import FIX.Fields.OptAttribute
import FIX.Fields.Pool
import FIX.Fields.Product
import FIX.Fields.RedemptionDate
import FIX.Fields.RepoCollateralSecurityType
import FIX.Fields.RepurchaseRate
import FIX.Fields.RepurchaseTerm
import FIX.Fields.SecurityDesc
import FIX.Fields.SecurityExchange
import FIX.Fields.SecurityID
import FIX.Fields.SecurityIDSource
import FIX.Fields.SecuritySubType
import FIX.Fields.SecurityType
import FIX.Fields.StateOrProvinceOfIssue
import FIX.Fields.StrikeCurrency
import FIX.Fields.StrikePrice
import FIX.Fields.Symbol
import FIX.Fields.SymbolSfx
import FIX.Fields.UPICode
import FIX.Fields.UPICode2
import FIX.Groups.Class
import FIX.Groups.EventsGroupElem
import FIX.Groups.SecurityAltIDGroupElem
import GHC.Generics (Generic)

-- | ComponentSpec
--   { componentName = "Instrument"
--   , componentPieces =
--       [ MessagePieceField "Symbol" True
--       , MessagePieceField "SymbolSfx" False
--       , MessagePieceField "SecurityID" False
--       , MessagePieceField "SecurityIDSource" False
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "NoSecurityAltID"
--             , groupNumberField = "NoSecurityAltID"
--             , groupPieces =
--                 [ MessagePieceField "SecurityAltID" True
--                 , MessagePieceField "SecurityAltIDSource" False
--                 ]
--             }
--           False
--       , MessagePieceField "Product" False
--       , MessagePieceField "CFICode" False
--       , MessagePieceField "UPICode" False
--       , MessagePieceField "UPICode2" False
--       , MessagePieceField "SecurityType" False
--       , MessagePieceField "SecuritySubType" False
--       , MessagePieceField "MaturityMonthYear" False
--       , MessagePieceField "MaturityDate" False
--       , MessagePieceField "MaturityDate2" False
--       , MessagePieceField "CouponPaymentDate" False
--       , MessagePieceField "IssueDate" False
--       , MessagePieceField "RepoCollateralSecurityType" False
--       , MessagePieceField "RepurchaseTerm" False
--       , MessagePieceField "RepurchaseRate" False
--       , MessagePieceField "Factor" False
--       , MessagePieceField "CreditRating" False
--       , MessagePieceField "InstrRegistry" False
--       , MessagePieceField "CountryOfIssue" False
--       , MessagePieceField "StateOrProvinceOfIssue" False
--       , MessagePieceField "LocaleOfIssue" False
--       , MessagePieceField "RedemptionDate" False
--       , MessagePieceField "StrikePrice" False
--       , MessagePieceField "StrikeCurrency" False
--       , MessagePieceField "OptAttribute" False
--       , MessagePieceField "ContractMultiplier" False
--       , MessagePieceField "CouponRate" False
--       , MessagePieceField "SecurityExchange" False
--       , MessagePieceField "Issuer" False
--       , MessagePieceField "EncodedIssuer" False
--       , MessagePieceField "SecurityDesc" False
--       , MessagePieceField "EncodedSecurityDesc" False
--       , MessagePieceField "Pool" False
--       , MessagePieceField "ContractSettlMonth" False
--       , MessagePieceField "CPProgram" False
--       , MessagePieceField "CPRegType" False
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "NoEvents"
--             , groupNumberField = "NoEvents"
--             , groupPieces =
--                 [ MessagePieceField "EventType" True
--                 , MessagePieceField "EventDate" False
--                 , MessagePieceField "EventPx" False
--                 , MessagePieceField "EventText" False
--                 ]
--             }
--           False
--       , MessagePieceField "DatedDate" False
--       , MessagePieceField "InterestAccrualDate" False
--       ]
--   }
data Instrument = Instrument
  { instrumentSymbol :: !Symbol,
    instrumentSymbolSfx :: !(Maybe SymbolSfx),
    instrumentSecurityID :: !(Maybe SecurityID),
    instrumentSecurityIDSource :: !(Maybe SecurityIDSource),
    instrumentSecurityAltIDGroup :: ![SecurityAltIDGroupElem],
    instrumentProduct :: !(Maybe Product),
    instrumentCFICode :: !(Maybe CFICode),
    instrumentUPICode :: !(Maybe UPICode),
    instrumentUPICode2 :: !(Maybe UPICode2),
    instrumentSecurityType :: !(Maybe SecurityType),
    instrumentSecuritySubType :: !(Maybe SecuritySubType),
    instrumentMaturityMonthYear :: !(Maybe MaturityMonthYear),
    instrumentMaturityDate :: !(Maybe MaturityDate),
    instrumentMaturityDate2 :: !(Maybe MaturityDate2),
    instrumentCouponPaymentDate :: !(Maybe CouponPaymentDate),
    instrumentIssueDate :: !(Maybe IssueDate),
    instrumentRepoCollateralSecurityType :: !(Maybe RepoCollateralSecurityType),
    instrumentRepurchaseTerm :: !(Maybe RepurchaseTerm),
    instrumentRepurchaseRate :: !(Maybe RepurchaseRate),
    instrumentFactor :: !(Maybe Factor),
    instrumentCreditRating :: !(Maybe CreditRating),
    instrumentInstrRegistry :: !(Maybe InstrRegistry),
    instrumentCountryOfIssue :: !(Maybe CountryOfIssue),
    instrumentStateOrProvinceOfIssue :: !(Maybe StateOrProvinceOfIssue),
    instrumentLocaleOfIssue :: !(Maybe LocaleOfIssue),
    instrumentRedemptionDate :: !(Maybe RedemptionDate),
    instrumentStrikePrice :: !(Maybe StrikePrice),
    instrumentStrikeCurrency :: !(Maybe StrikeCurrency),
    instrumentOptAttribute :: !(Maybe OptAttribute),
    instrumentContractMultiplier :: !(Maybe ContractMultiplier),
    instrumentCouponRate :: !(Maybe CouponRate),
    instrumentSecurityExchange :: !(Maybe SecurityExchange),
    instrumentIssuer :: !(Maybe Issuer),
    instrumentEncodedIssuer :: !(Maybe EncodedIssuer),
    instrumentSecurityDesc :: !(Maybe SecurityDesc),
    instrumentEncodedSecurityDesc :: !(Maybe EncodedSecurityDesc),
    instrumentPool :: !(Maybe Pool),
    instrumentContractSettlMonth :: !(Maybe ContractSettlMonth),
    instrumentCPProgram :: !(Maybe CPProgram),
    instrumentCPRegType :: !(Maybe CPRegType),
    instrumentEventsGroup :: ![EventsGroupElem],
    instrumentDatedDate :: !(Maybe DatedDate),
    instrumentInterestAccrualDate :: !(Maybe InterestAccrualDate)
  }
  deriving stock (Show, Eq, Generic)

instance Validity Instrument

instance IsComponent Instrument where
  toComponentFields ((Instrument {..})) =
    mconcat
      [ requiredFieldB instrumentSymbol,
        optionalFieldB instrumentSymbolSfx,
        optionalFieldB instrumentSecurityID,
        optionalFieldB instrumentSecurityIDSource,
        optionalGroupB instrumentSecurityAltIDGroup,
        optionalFieldB instrumentProduct,
        optionalFieldB instrumentCFICode,
        optionalFieldB instrumentUPICode,
        optionalFieldB instrumentUPICode2,
        optionalFieldB instrumentSecurityType,
        optionalFieldB instrumentSecuritySubType,
        optionalFieldB instrumentMaturityMonthYear,
        optionalFieldB instrumentMaturityDate,
        optionalFieldB instrumentMaturityDate2,
        optionalFieldB instrumentCouponPaymentDate,
        optionalFieldB instrumentIssueDate,
        optionalFieldB instrumentRepoCollateralSecurityType,
        optionalFieldB instrumentRepurchaseTerm,
        optionalFieldB instrumentRepurchaseRate,
        optionalFieldB instrumentFactor,
        optionalFieldB instrumentCreditRating,
        optionalFieldB instrumentInstrRegistry,
        optionalFieldB instrumentCountryOfIssue,
        optionalFieldB instrumentStateOrProvinceOfIssue,
        optionalFieldB instrumentLocaleOfIssue,
        optionalFieldB instrumentRedemptionDate,
        optionalFieldB instrumentStrikePrice,
        optionalFieldB instrumentStrikeCurrency,
        optionalFieldB instrumentOptAttribute,
        optionalFieldB instrumentContractMultiplier,
        optionalFieldB instrumentCouponRate,
        optionalFieldB instrumentSecurityExchange,
        optionalFieldB instrumentIssuer,
        optionalFieldB instrumentEncodedIssuer,
        optionalFieldB instrumentSecurityDesc,
        optionalFieldB instrumentEncodedSecurityDesc,
        optionalFieldB instrumentPool,
        optionalFieldB instrumentContractSettlMonth,
        optionalFieldB instrumentCPProgram,
        optionalFieldB instrumentCPRegType,
        optionalGroupB instrumentEventsGroup,
        optionalFieldB instrumentDatedDate,
        optionalFieldB instrumentInterestAccrualDate
      ]
  fromComponentFields = do
    instrumentSymbol <- requiredFieldP
    instrumentSymbolSfx <- optionalFieldP
    instrumentSecurityID <- optionalFieldP
    instrumentSecurityIDSource <- optionalFieldP
    instrumentSecurityAltIDGroup <- optionalGroupP
    instrumentProduct <- optionalFieldP
    instrumentCFICode <- optionalFieldP
    instrumentUPICode <- optionalFieldP
    instrumentUPICode2 <- optionalFieldP
    instrumentSecurityType <- optionalFieldP
    instrumentSecuritySubType <- optionalFieldP
    instrumentMaturityMonthYear <- optionalFieldP
    instrumentMaturityDate <- optionalFieldP
    instrumentMaturityDate2 <- optionalFieldP
    instrumentCouponPaymentDate <- optionalFieldP
    instrumentIssueDate <- optionalFieldP
    instrumentRepoCollateralSecurityType <- optionalFieldP
    instrumentRepurchaseTerm <- optionalFieldP
    instrumentRepurchaseRate <- optionalFieldP
    instrumentFactor <- optionalFieldP
    instrumentCreditRating <- optionalFieldP
    instrumentInstrRegistry <- optionalFieldP
    instrumentCountryOfIssue <- optionalFieldP
    instrumentStateOrProvinceOfIssue <- optionalFieldP
    instrumentLocaleOfIssue <- optionalFieldP
    instrumentRedemptionDate <- optionalFieldP
    instrumentStrikePrice <- optionalFieldP
    instrumentStrikeCurrency <- optionalFieldP
    instrumentOptAttribute <- optionalFieldP
    instrumentContractMultiplier <- optionalFieldP
    instrumentCouponRate <- optionalFieldP
    instrumentSecurityExchange <- optionalFieldP
    instrumentIssuer <- optionalFieldP
    instrumentEncodedIssuer <- optionalFieldP
    instrumentSecurityDesc <- optionalFieldP
    instrumentEncodedSecurityDesc <- optionalFieldP
    instrumentPool <- optionalFieldP
    instrumentContractSettlMonth <- optionalFieldP
    instrumentCPProgram <- optionalFieldP
    instrumentCPRegType <- optionalFieldP
    instrumentEventsGroup <- optionalGroupP
    instrumentDatedDate <- optionalFieldP
    instrumentInterestAccrualDate <- optionalFieldP
    pure (Instrument {..})

makeInstrument :: Symbol -> Instrument
makeInstrument instrumentSymbol =
  let instrumentSymbolSfx = Nothing
      instrumentSecurityID = Nothing
      instrumentSecurityIDSource = Nothing
      instrumentSecurityAltIDGroup = []
      instrumentProduct = Nothing
      instrumentCFICode = Nothing
      instrumentUPICode = Nothing
      instrumentUPICode2 = Nothing
      instrumentSecurityType = Nothing
      instrumentSecuritySubType = Nothing
      instrumentMaturityMonthYear = Nothing
      instrumentMaturityDate = Nothing
      instrumentMaturityDate2 = Nothing
      instrumentCouponPaymentDate = Nothing
      instrumentIssueDate = Nothing
      instrumentRepoCollateralSecurityType = Nothing
      instrumentRepurchaseTerm = Nothing
      instrumentRepurchaseRate = Nothing
      instrumentFactor = Nothing
      instrumentCreditRating = Nothing
      instrumentInstrRegistry = Nothing
      instrumentCountryOfIssue = Nothing
      instrumentStateOrProvinceOfIssue = Nothing
      instrumentLocaleOfIssue = Nothing
      instrumentRedemptionDate = Nothing
      instrumentStrikePrice = Nothing
      instrumentStrikeCurrency = Nothing
      instrumentOptAttribute = Nothing
      instrumentContractMultiplier = Nothing
      instrumentCouponRate = Nothing
      instrumentSecurityExchange = Nothing
      instrumentIssuer = Nothing
      instrumentEncodedIssuer = Nothing
      instrumentSecurityDesc = Nothing
      instrumentEncodedSecurityDesc = Nothing
      instrumentPool = Nothing
      instrumentContractSettlMonth = Nothing
      instrumentCPProgram = Nothing
      instrumentCPRegType = Nothing
      instrumentEventsGroup = []
      instrumentDatedDate = Nothing
      instrumentInterestAccrualDate = Nothing
   in (Instrument {..})
