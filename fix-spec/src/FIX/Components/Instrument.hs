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
import FIX.Components.EvntGrp
import FIX.Components.SecAltIDGrp
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
import FIX.Fields.MaturityMonthYear
import FIX.Fields.MsgType
import FIX.Fields.OptAttribute
import FIX.Fields.Pool
import FIX.Fields.Product
import FIX.Fields.PutOrCall
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
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | ComponentSpec
--   { componentName = "Instrument"
--   , componentPieces =
--       [ MessagePieceField "Symbol" False
--       , MessagePieceField "SymbolSfx" False
--       , MessagePieceField "SecurityID" False
--       , MessagePieceField "SecurityIDSource" False
--       , MessagePieceComponent "SecAltIDGrp" False
--       , MessagePieceField "Product" False
--       , MessagePieceField "CFICode" False
--       , MessagePieceField "SecurityType" False
--       , MessagePieceField "SecuritySubType" False
--       , MessagePieceField "MaturityMonthYear" False
--       , MessagePieceField "MaturityDate" False
--       , MessagePieceField "PutOrCall" False
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
--       , MessagePieceComponent "EvntGrp" False
--       , MessagePieceField "DatedDate" False
--       , MessagePieceField "InterestAccrualDate" False
--       ]
--   }
data Instrument = Instrument
  { instrumentSymbol :: !(Maybe Symbol),
    instrumentSymbolSfx :: !(Maybe SymbolSfx),
    instrumentSecurityID :: !(Maybe SecurityID),
    instrumentSecurityIDSource :: !(Maybe SecurityIDSource),
    instrumentSecAltIDGrp :: !(Maybe SecAltIDGrp),
    instrumentProduct :: !(Maybe Product),
    instrumentCFICode :: !(Maybe CFICode),
    instrumentSecurityType :: !(Maybe SecurityType),
    instrumentSecuritySubType :: !(Maybe SecuritySubType),
    instrumentMaturityMonthYear :: !(Maybe MaturityMonthYear),
    instrumentMaturityDate :: !(Maybe MaturityDate),
    instrumentPutOrCall :: !(Maybe PutOrCall),
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
    instrumentEvntGrp :: !(Maybe EvntGrp),
    instrumentDatedDate :: !(Maybe DatedDate),
    instrumentInterestAccrualDate :: !(Maybe InterestAccrualDate)
  }
  deriving stock (Show, Eq, Generic)

instance Validity Instrument

instance IsComponent Instrument where
  toComponentFields ((Instrument {..})) =
    mconcat
      [ optionalFieldB instrumentSymbol,
        optionalFieldB instrumentSymbolSfx,
        optionalFieldB instrumentSecurityID,
        optionalFieldB instrumentSecurityIDSource,
        optionalComponentB instrumentSecAltIDGrp,
        optionalFieldB instrumentProduct,
        optionalFieldB instrumentCFICode,
        optionalFieldB instrumentSecurityType,
        optionalFieldB instrumentSecuritySubType,
        optionalFieldB instrumentMaturityMonthYear,
        optionalFieldB instrumentMaturityDate,
        optionalFieldB instrumentPutOrCall,
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
        optionalComponentB instrumentEvntGrp,
        optionalFieldB instrumentDatedDate,
        optionalFieldB instrumentInterestAccrualDate
      ]
  fromComponentFields = do
    instrumentSymbol <- optionalFieldP
    instrumentSymbolSfx <- optionalFieldP
    instrumentSecurityID <- optionalFieldP
    instrumentSecurityIDSource <- optionalFieldP
    instrumentSecAltIDGrp <- optionalComponentP
    instrumentProduct <- optionalFieldP
    instrumentCFICode <- optionalFieldP
    instrumentSecurityType <- optionalFieldP
    instrumentSecuritySubType <- optionalFieldP
    instrumentMaturityMonthYear <- optionalFieldP
    instrumentMaturityDate <- optionalFieldP
    instrumentPutOrCall <- optionalFieldP
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
    instrumentEvntGrp <- optionalComponentP
    instrumentDatedDate <- optionalFieldP
    instrumentInterestAccrualDate <- optionalFieldP
    pure (Instrument {..})
