{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Components.InstrumentLeg where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.EncodedLegIssuer
import FIX.Fields.EncodedLegSecurityDesc
import FIX.Fields.LegCFICode
import FIX.Fields.LegContractMultiplier
import FIX.Fields.LegContractSettlMonth
import FIX.Fields.LegCountryOfIssue
import FIX.Fields.LegCouponPaymentDate
import FIX.Fields.LegCouponRate
import FIX.Fields.LegCreditRating
import FIX.Fields.LegCurrency
import FIX.Fields.LegDatedDate
import FIX.Fields.LegFactor
import FIX.Fields.LegInstrRegistry
import FIX.Fields.LegInterestAccrualDate
import FIX.Fields.LegIssueDate
import FIX.Fields.LegIssuer
import FIX.Fields.LegLocaleOfIssue
import FIX.Fields.LegMaturityDate
import FIX.Fields.LegMaturityMonthYear
import FIX.Fields.LegOptAttribute
import FIX.Fields.LegPool
import FIX.Fields.LegProduct
import FIX.Fields.LegRatioQty
import FIX.Fields.LegRedemptionDate
import FIX.Fields.LegRepoCollateralSecurityType
import FIX.Fields.LegRepurchaseRate
import FIX.Fields.LegRepurchaseTerm
import FIX.Fields.LegSecurityDesc
import FIX.Fields.LegSecurityExchange
import FIX.Fields.LegSecurityID
import FIX.Fields.LegSecurityIDSource
import FIX.Fields.LegSecuritySubType
import FIX.Fields.LegSecurityType
import FIX.Fields.LegSide
import FIX.Fields.LegStateOrProvinceOfIssue
import FIX.Fields.LegStrikeCurrency
import FIX.Fields.LegStrikePrice
import FIX.Fields.LegSymbol
import FIX.Fields.LegSymbolSfx
import FIX.Fields.LegUPICode
import FIX.Fields.MsgType
import FIX.Groups.Class
import FIX.Groups.InstrumentLegLegSecurityAltIDGroupElem
import GHC.Generics (Generic)

-- | ComponentSpec
--   { componentName = "InstrumentLeg"
--   , componentPieces =
--       [ MessagePieceField "LegSymbol" False
--       , MessagePieceField "LegSymbolSfx" False
--       , MessagePieceField "LegSecurityID" False
--       , MessagePieceField "LegSecurityIDSource" False
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "InstrumentLegLegSecurityAltID"
--             , groupNumberField = "NoLegSecurityAltID"
--             , groupPieces =
--                 [ MessagePieceField "LegSecurityAltID" True
--                 , MessagePieceField "LegSecurityAltIDSource" False
--                 ]
--             }
--           False
--       , MessagePieceField "LegProduct" False
--       , MessagePieceField "LegCFICode" False
--       , MessagePieceField "LegUPICode" False
--       , MessagePieceField "LegSecurityType" False
--       , MessagePieceField "LegSecuritySubType" False
--       , MessagePieceField "LegMaturityMonthYear" False
--       , MessagePieceField "LegMaturityDate" False
--       , MessagePieceField "LegCouponPaymentDate" False
--       , MessagePieceField "LegIssueDate" False
--       , MessagePieceField "LegRepoCollateralSecurityType" False
--       , MessagePieceField "LegRepurchaseTerm" False
--       , MessagePieceField "LegRepurchaseRate" False
--       , MessagePieceField "LegFactor" False
--       , MessagePieceField "LegCreditRating" False
--       , MessagePieceField "LegInstrRegistry" False
--       , MessagePieceField "LegCountryOfIssue" False
--       , MessagePieceField "LegStateOrProvinceOfIssue" False
--       , MessagePieceField "LegLocaleOfIssue" False
--       , MessagePieceField "LegRedemptionDate" False
--       , MessagePieceField "LegStrikePrice" False
--       , MessagePieceField "LegStrikeCurrency" False
--       , MessagePieceField "LegOptAttribute" False
--       , MessagePieceField "LegContractMultiplier" False
--       , MessagePieceField "LegCouponRate" False
--       , MessagePieceField "LegSecurityExchange" False
--       , MessagePieceField "LegIssuer" False
--       , MessagePieceField "EncodedLegIssuer" False
--       , MessagePieceField "LegSecurityDesc" False
--       , MessagePieceField "EncodedLegSecurityDesc" False
--       , MessagePieceField "LegRatioQty" False
--       , MessagePieceField "LegSide" False
--       , MessagePieceField "LegCurrency" False
--       , MessagePieceField "LegPool" False
--       , MessagePieceField "LegDatedDate" False
--       , MessagePieceField "LegContractSettlMonth" False
--       , MessagePieceField "LegInterestAccrualDate" False
--       ]
--   }
data InstrumentLeg = InstrumentLeg
  { instrumentLegLegSymbol :: !(Maybe LegSymbol),
    instrumentLegLegSymbolSfx :: !(Maybe LegSymbolSfx),
    instrumentLegLegSecurityID :: !(Maybe LegSecurityID),
    instrumentLegLegSecurityIDSource :: !(Maybe LegSecurityIDSource),
    instrumentLegInstrumentLegLegSecurityAltIDGroup :: ![InstrumentLegLegSecurityAltIDGroupElem],
    instrumentLegLegProduct :: !(Maybe LegProduct),
    instrumentLegLegCFICode :: !(Maybe LegCFICode),
    instrumentLegLegUPICode :: !(Maybe LegUPICode),
    instrumentLegLegSecurityType :: !(Maybe LegSecurityType),
    instrumentLegLegSecuritySubType :: !(Maybe LegSecuritySubType),
    instrumentLegLegMaturityMonthYear :: !(Maybe LegMaturityMonthYear),
    instrumentLegLegMaturityDate :: !(Maybe LegMaturityDate),
    instrumentLegLegCouponPaymentDate :: !(Maybe LegCouponPaymentDate),
    instrumentLegLegIssueDate :: !(Maybe LegIssueDate),
    instrumentLegLegRepoCollateralSecurityType :: !(Maybe LegRepoCollateralSecurityType),
    instrumentLegLegRepurchaseTerm :: !(Maybe LegRepurchaseTerm),
    instrumentLegLegRepurchaseRate :: !(Maybe LegRepurchaseRate),
    instrumentLegLegFactor :: !(Maybe LegFactor),
    instrumentLegLegCreditRating :: !(Maybe LegCreditRating),
    instrumentLegLegInstrRegistry :: !(Maybe LegInstrRegistry),
    instrumentLegLegCountryOfIssue :: !(Maybe LegCountryOfIssue),
    instrumentLegLegStateOrProvinceOfIssue :: !(Maybe LegStateOrProvinceOfIssue),
    instrumentLegLegLocaleOfIssue :: !(Maybe LegLocaleOfIssue),
    instrumentLegLegRedemptionDate :: !(Maybe LegRedemptionDate),
    instrumentLegLegStrikePrice :: !(Maybe LegStrikePrice),
    instrumentLegLegStrikeCurrency :: !(Maybe LegStrikeCurrency),
    instrumentLegLegOptAttribute :: !(Maybe LegOptAttribute),
    instrumentLegLegContractMultiplier :: !(Maybe LegContractMultiplier),
    instrumentLegLegCouponRate :: !(Maybe LegCouponRate),
    instrumentLegLegSecurityExchange :: !(Maybe LegSecurityExchange),
    instrumentLegLegIssuer :: !(Maybe LegIssuer),
    instrumentLegEncodedLegIssuer :: !(Maybe EncodedLegIssuer),
    instrumentLegLegSecurityDesc :: !(Maybe LegSecurityDesc),
    instrumentLegEncodedLegSecurityDesc :: !(Maybe EncodedLegSecurityDesc),
    instrumentLegLegRatioQty :: !(Maybe LegRatioQty),
    instrumentLegLegSide :: !(Maybe LegSide),
    instrumentLegLegCurrency :: !(Maybe LegCurrency),
    instrumentLegLegPool :: !(Maybe LegPool),
    instrumentLegLegDatedDate :: !(Maybe LegDatedDate),
    instrumentLegLegContractSettlMonth :: !(Maybe LegContractSettlMonth),
    instrumentLegLegInterestAccrualDate :: !(Maybe LegInterestAccrualDate)
  }
  deriving stock (Show, Eq, Generic)

instance Validity InstrumentLeg

instance IsComponent InstrumentLeg where
  toComponentFields ((InstrumentLeg {..})) =
    mconcat
      [ optionalFieldB instrumentLegLegSymbol,
        optionalFieldB instrumentLegLegSymbolSfx,
        optionalFieldB instrumentLegLegSecurityID,
        optionalFieldB instrumentLegLegSecurityIDSource,
        optionalGroupB instrumentLegInstrumentLegLegSecurityAltIDGroup,
        optionalFieldB instrumentLegLegProduct,
        optionalFieldB instrumentLegLegCFICode,
        optionalFieldB instrumentLegLegUPICode,
        optionalFieldB instrumentLegLegSecurityType,
        optionalFieldB instrumentLegLegSecuritySubType,
        optionalFieldB instrumentLegLegMaturityMonthYear,
        optionalFieldB instrumentLegLegMaturityDate,
        optionalFieldB instrumentLegLegCouponPaymentDate,
        optionalFieldB instrumentLegLegIssueDate,
        optionalFieldB instrumentLegLegRepoCollateralSecurityType,
        optionalFieldB instrumentLegLegRepurchaseTerm,
        optionalFieldB instrumentLegLegRepurchaseRate,
        optionalFieldB instrumentLegLegFactor,
        optionalFieldB instrumentLegLegCreditRating,
        optionalFieldB instrumentLegLegInstrRegistry,
        optionalFieldB instrumentLegLegCountryOfIssue,
        optionalFieldB instrumentLegLegStateOrProvinceOfIssue,
        optionalFieldB instrumentLegLegLocaleOfIssue,
        optionalFieldB instrumentLegLegRedemptionDate,
        optionalFieldB instrumentLegLegStrikePrice,
        optionalFieldB instrumentLegLegStrikeCurrency,
        optionalFieldB instrumentLegLegOptAttribute,
        optionalFieldB instrumentLegLegContractMultiplier,
        optionalFieldB instrumentLegLegCouponRate,
        optionalFieldB instrumentLegLegSecurityExchange,
        optionalFieldB instrumentLegLegIssuer,
        optionalFieldB instrumentLegEncodedLegIssuer,
        optionalFieldB instrumentLegLegSecurityDesc,
        optionalFieldB instrumentLegEncodedLegSecurityDesc,
        optionalFieldB instrumentLegLegRatioQty,
        optionalFieldB instrumentLegLegSide,
        optionalFieldB instrumentLegLegCurrency,
        optionalFieldB instrumentLegLegPool,
        optionalFieldB instrumentLegLegDatedDate,
        optionalFieldB instrumentLegLegContractSettlMonth,
        optionalFieldB instrumentLegLegInterestAccrualDate
      ]
  fromComponentFields = do
    instrumentLegLegSymbol <- optionalFieldP
    instrumentLegLegSymbolSfx <- optionalFieldP
    instrumentLegLegSecurityID <- optionalFieldP
    instrumentLegLegSecurityIDSource <- optionalFieldP
    instrumentLegInstrumentLegLegSecurityAltIDGroup <- optionalGroupP
    instrumentLegLegProduct <- optionalFieldP
    instrumentLegLegCFICode <- optionalFieldP
    instrumentLegLegUPICode <- optionalFieldP
    instrumentLegLegSecurityType <- optionalFieldP
    instrumentLegLegSecuritySubType <- optionalFieldP
    instrumentLegLegMaturityMonthYear <- optionalFieldP
    instrumentLegLegMaturityDate <- optionalFieldP
    instrumentLegLegCouponPaymentDate <- optionalFieldP
    instrumentLegLegIssueDate <- optionalFieldP
    instrumentLegLegRepoCollateralSecurityType <- optionalFieldP
    instrumentLegLegRepurchaseTerm <- optionalFieldP
    instrumentLegLegRepurchaseRate <- optionalFieldP
    instrumentLegLegFactor <- optionalFieldP
    instrumentLegLegCreditRating <- optionalFieldP
    instrumentLegLegInstrRegistry <- optionalFieldP
    instrumentLegLegCountryOfIssue <- optionalFieldP
    instrumentLegLegStateOrProvinceOfIssue <- optionalFieldP
    instrumentLegLegLocaleOfIssue <- optionalFieldP
    instrumentLegLegRedemptionDate <- optionalFieldP
    instrumentLegLegStrikePrice <- optionalFieldP
    instrumentLegLegStrikeCurrency <- optionalFieldP
    instrumentLegLegOptAttribute <- optionalFieldP
    instrumentLegLegContractMultiplier <- optionalFieldP
    instrumentLegLegCouponRate <- optionalFieldP
    instrumentLegLegSecurityExchange <- optionalFieldP
    instrumentLegLegIssuer <- optionalFieldP
    instrumentLegEncodedLegIssuer <- optionalFieldP
    instrumentLegLegSecurityDesc <- optionalFieldP
    instrumentLegEncodedLegSecurityDesc <- optionalFieldP
    instrumentLegLegRatioQty <- optionalFieldP
    instrumentLegLegSide <- optionalFieldP
    instrumentLegLegCurrency <- optionalFieldP
    instrumentLegLegPool <- optionalFieldP
    instrumentLegLegDatedDate <- optionalFieldP
    instrumentLegLegContractSettlMonth <- optionalFieldP
    instrumentLegLegInterestAccrualDate <- optionalFieldP
    pure (InstrumentLeg {..})

makeInstrumentLeg :: InstrumentLeg
makeInstrumentLeg =
  let instrumentLegLegSymbol = Nothing
      instrumentLegLegSymbolSfx = Nothing
      instrumentLegLegSecurityID = Nothing
      instrumentLegLegSecurityIDSource = Nothing
      instrumentLegInstrumentLegLegSecurityAltIDGroup = []
      instrumentLegLegProduct = Nothing
      instrumentLegLegCFICode = Nothing
      instrumentLegLegUPICode = Nothing
      instrumentLegLegSecurityType = Nothing
      instrumentLegLegSecuritySubType = Nothing
      instrumentLegLegMaturityMonthYear = Nothing
      instrumentLegLegMaturityDate = Nothing
      instrumentLegLegCouponPaymentDate = Nothing
      instrumentLegLegIssueDate = Nothing
      instrumentLegLegRepoCollateralSecurityType = Nothing
      instrumentLegLegRepurchaseTerm = Nothing
      instrumentLegLegRepurchaseRate = Nothing
      instrumentLegLegFactor = Nothing
      instrumentLegLegCreditRating = Nothing
      instrumentLegLegInstrRegistry = Nothing
      instrumentLegLegCountryOfIssue = Nothing
      instrumentLegLegStateOrProvinceOfIssue = Nothing
      instrumentLegLegLocaleOfIssue = Nothing
      instrumentLegLegRedemptionDate = Nothing
      instrumentLegLegStrikePrice = Nothing
      instrumentLegLegStrikeCurrency = Nothing
      instrumentLegLegOptAttribute = Nothing
      instrumentLegLegContractMultiplier = Nothing
      instrumentLegLegCouponRate = Nothing
      instrumentLegLegSecurityExchange = Nothing
      instrumentLegLegIssuer = Nothing
      instrumentLegEncodedLegIssuer = Nothing
      instrumentLegLegSecurityDesc = Nothing
      instrumentLegEncodedLegSecurityDesc = Nothing
      instrumentLegLegRatioQty = Nothing
      instrumentLegLegSide = Nothing
      instrumentLegLegCurrency = Nothing
      instrumentLegLegPool = Nothing
      instrumentLegLegDatedDate = Nothing
      instrumentLegLegContractSettlMonth = Nothing
      instrumentLegLegInterestAccrualDate = Nothing
   in (InstrumentLeg {..})
