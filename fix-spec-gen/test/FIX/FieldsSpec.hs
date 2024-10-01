{-# LANGUAGE TypeApplications #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.FieldsSpec where

import FIX.Core.TestUtils
import FIX.Fields.Account
import FIX.Fields.AccountType
import FIX.Fields.AccruedInterestAmt
import FIX.Fields.AccruedInterestRate
import FIX.Fields.AcctIDSource
import FIX.Fields.AgreementCurrency
import FIX.Fields.AgreementDate
import FIX.Fields.AgreementDesc
import FIX.Fields.AgreementID
import FIX.Fields.AllocAccount
import FIX.Fields.AllocAcctIDSource
import FIX.Fields.AllocID
import FIX.Fields.AllocQty
import FIX.Fields.AllocSettlCurrency
import FIX.Fields.AvgPx
import FIX.Fields.AvgPxPrecision
import FIX.Fields.BasisFeatureDate
import FIX.Fields.BasisFeaturePrice
import FIX.Fields.BeginSeqNo
import FIX.Fields.BeginString
import FIX.Fields.BenchmarkCurveCurrency
import FIX.Fields.BenchmarkCurveName
import FIX.Fields.BenchmarkCurvePoint
import FIX.Fields.BenchmarkPrice
import FIX.Fields.BenchmarkPriceType
import FIX.Fields.BenchmarkSecurityID
import FIX.Fields.BenchmarkSecurityIDSource
import FIX.Fields.BidExAnteCost
import FIX.Fields.BidExAnteCostPercentage
import FIX.Fields.BidForwardPoints
import FIX.Fields.BidForwardPoints2
import FIX.Fields.BidInterestAtMaturity
import FIX.Fields.BidPx
import FIX.Fields.BidPx2
import FIX.Fields.BidSize
import FIX.Fields.BidSpotRate
import FIX.Fields.BidYield
import FIX.Fields.BodyLength
import FIX.Fields.BookingType
import FIX.Fields.BookingUnit
import FIX.Fields.BusinessRejectReason
import FIX.Fields.BusinessRejectRefID
import FIX.Fields.CFICode
import FIX.Fields.CPProgram
import FIX.Fields.CPRegType
import FIX.Fields.CancellationRights
import FIX.Fields.CashMargin
import FIX.Fields.CashOrderQty
import FIX.Fields.CheckSum
import FIX.Fields.ClOrdID
import FIX.Fields.ClOrdLinkID
import FIX.Fields.ClearingFeeIndicator
import FIX.Fields.CommCurrency
import FIX.Fields.CommType
import FIX.Fields.Commission
import FIX.Fields.ComplianceID
import FIX.Fields.Concession
import FIX.Fields.ContAmtCurr
import FIX.Fields.ContAmtType
import FIX.Fields.ContAmtValue
import FIX.Fields.ContraBroker
import FIX.Fields.ContraLegRefID
import FIX.Fields.ContraTradeQty
import FIX.Fields.ContraTradeTime
import FIX.Fields.ContraTrader
import FIX.Fields.ContractMultiplier
import FIX.Fields.ContractSettlMonth
import FIX.Fields.CopyMsgIndicator
import FIX.Fields.CountryOfIssue
import FIX.Fields.CouponPaymentDate
import FIX.Fields.CouponRate
import FIX.Fields.CoveredOrUncovered
import FIX.Fields.CreditRating
import FIX.Fields.CrossID
import FIX.Fields.CrossType
import FIX.Fields.CstmApplVerID
import FIX.Fields.CumQty
import FIX.Fields.Currency
import FIX.Fields.CustOrderCapacity
import FIX.Fields.CustomFieldsContext
import FIX.Fields.CustomFieldsName
import FIX.Fields.CustomFieldsValue
import FIX.Fields.DatedDate
import FIX.Fields.DayAvgPx
import FIX.Fields.DayBookingInst
import FIX.Fields.DayCount
import FIX.Fields.DayCumQty
import FIX.Fields.DayOrderQty
import FIX.Fields.DeliveryType
import FIX.Fields.Designation
import FIX.Fields.DiscretionInst
import FIX.Fields.DiscretionLimitType
import FIX.Fields.DiscretionMoveType
import FIX.Fields.DiscretionOffsetType
import FIX.Fields.DiscretionOffsetValue
import FIX.Fields.DiscretionPrice
import FIX.Fields.DiscretionRoundDirection
import FIX.Fields.DiscretionScope
import FIX.Fields.EffectiveTime
import FIX.Fields.EncodedIssuer
import FIX.Fields.EncodedLegIssuer
import FIX.Fields.EncodedLegSecurityDesc
import FIX.Fields.EncodedSecurityDesc
import FIX.Fields.EncodedText
import FIX.Fields.EncryptMethod
import FIX.Fields.EndAccruedInterestAmt
import FIX.Fields.EndCash
import FIX.Fields.EndDate
import FIX.Fields.EndSeqNo
import FIX.Fields.EventDate
import FIX.Fields.EventPx
import FIX.Fields.EventText
import FIX.Fields.EventType
import FIX.Fields.ExDate
import FIX.Fields.ExDestination
import FIX.Fields.ExecID
import FIX.Fields.ExecInst
import FIX.Fields.ExecPriceAdjustment
import FIX.Fields.ExecPriceType
import FIX.Fields.ExecRefID
import FIX.Fields.ExecRestatementReason
import FIX.Fields.ExecType
import FIX.Fields.ExecValuationPoint
import FIX.Fields.ExecutionVenueType
import FIX.Fields.ExpireDate
import FIX.Fields.ExpireTime
import FIX.Fields.Factor
import FIX.Fields.FarLegRatePrecision
import FIX.Fields.Fiduciary
import FIX.Fields.FixingCode
import FIX.Fields.FixingDate
import FIX.Fields.FixingReference
import FIX.Fields.FixingTime
import FIX.Fields.ForexReq
import FIX.Fields.ForwardPointsPrecision
import FIX.Fields.ForwardRatePrecision
import FIX.Fields.FundRenewWaiv
import FIX.Fields.GTBookingInst
import FIX.Fields.Gen ()
import FIX.Fields.GrossTradeAmt
import FIX.Fields.HandlInst
import FIX.Fields.Headline
import FIX.Fields.HeartBtInt
import FIX.Fields.IOIid
import FIX.Fields.IndividualAllocID
import FIX.Fields.InstrRegistry
import FIX.Fields.InterestAccrualDate
import FIX.Fields.InterestAtMaturity
import FIX.Fields.InterestRatePrecision
import FIX.Fields.InterestSettlType
import FIX.Fields.IssueDate
import FIX.Fields.Issuer
import FIX.Fields.LastCapacity
import FIX.Fields.LastForwardPoints
import FIX.Fields.LastForwardPoints2
import FIX.Fields.LastLiquidityInd
import FIX.Fields.LastMkt
import FIX.Fields.LastParPx
import FIX.Fields.LastPx
import FIX.Fields.LastPx2
import FIX.Fields.LastQty
import FIX.Fields.LastRptRequested
import FIX.Fields.LastSpotRate
import FIX.Fields.LeavesQty
import FIX.Fields.LegAllocAccount
import FIX.Fields.LegAllocAcctIDSource
import FIX.Fields.LegAllocQty
import FIX.Fields.LegBenchmarkCurveCurrency
import FIX.Fields.LegBenchmarkCurveName
import FIX.Fields.LegBenchmarkCurvePoint
import FIX.Fields.LegBenchmarkPrice
import FIX.Fields.LegBenchmarkPriceType
import FIX.Fields.LegBidExAnteCost
import FIX.Fields.LegBidExAnteCostPercentage
import FIX.Fields.LegBidPx
import FIX.Fields.LegCFICode
import FIX.Fields.LegContractMultiplier
import FIX.Fields.LegContractSettlMonth
import FIX.Fields.LegCountryOfIssue
import FIX.Fields.LegCouponPaymentDate
import FIX.Fields.LegCouponRate
import FIX.Fields.LegCoveredOrUncovered
import FIX.Fields.LegCreditRating
import FIX.Fields.LegCurrency
import FIX.Fields.LegDatedDate
import FIX.Fields.LegFactor
import FIX.Fields.LegIndividualAllocID
import FIX.Fields.LegInstrRegistry
import FIX.Fields.LegInterestAccrualDate
import FIX.Fields.LegIssueDate
import FIX.Fields.LegIssuer
import FIX.Fields.LegLocaleOfIssue
import FIX.Fields.LegMaturityDate
import FIX.Fields.LegMaturityMonthYear
import FIX.Fields.LegMidPx
import FIX.Fields.LegOfferExAnteCost
import FIX.Fields.LegOfferExAnteCostPercentage
import FIX.Fields.LegOfferPx
import FIX.Fields.LegOptAttribute
import FIX.Fields.LegPool
import FIX.Fields.LegPositionEffect
import FIX.Fields.LegPrice
import FIX.Fields.LegPriceType
import FIX.Fields.LegProduct
import FIX.Fields.LegQty
import FIX.Fields.LegRatioQty
import FIX.Fields.LegRedemptionDate
import FIX.Fields.LegRefID
import FIX.Fields.LegRepoCollateralSecurityType
import FIX.Fields.LegRepurchaseRate
import FIX.Fields.LegRepurchaseTerm
import FIX.Fields.LegSecurityAltID
import FIX.Fields.LegSecurityAltIDSource
import FIX.Fields.LegSecurityDesc
import FIX.Fields.LegSecurityExchange
import FIX.Fields.LegSecurityID
import FIX.Fields.LegSecurityIDSource
import FIX.Fields.LegSecuritySubType
import FIX.Fields.LegSecurityType
import FIX.Fields.LegSettlCurrency
import FIX.Fields.LegSettlDate
import FIX.Fields.LegSettlType
import FIX.Fields.LegSide
import FIX.Fields.LegSplitSettlDate
import FIX.Fields.LegStateOrProvinceOfIssue
import FIX.Fields.LegStipulationType
import FIX.Fields.LegStipulationValue
import FIX.Fields.LegStrikeCurrency
import FIX.Fields.LegStrikePrice
import FIX.Fields.LegSwapType
import FIX.Fields.LegSymbol
import FIX.Fields.LegSymbolSfx
import FIX.Fields.LegUPICode
import FIX.Fields.LinesOfText
import FIX.Fields.ListID
import FIX.Fields.LocaleOfIssue
import FIX.Fields.LocateReqd
import FIX.Fields.MarginRatio
import FIX.Fields.MassStatusReqID
import FIX.Fields.MaturityDate
import FIX.Fields.MaturityDate2
import FIX.Fields.MaturityMonthYear
import FIX.Fields.MaxFloor
import FIX.Fields.MaxShow
import FIX.Fields.MidPx
import FIX.Fields.MidPx2
import FIX.Fields.MidSpotRate
import FIX.Fields.MidYield
import FIX.Fields.MinBidSize
import FIX.Fields.MinOfferSize
import FIX.Fields.MinQty
import FIX.Fields.MiscFeeAmt
import FIX.Fields.MiscFeeBasis
import FIX.Fields.MiscFeeCurr
import FIX.Fields.MiscFeeType
import FIX.Fields.MktBidPx
import FIX.Fields.MktOfferPx
import FIX.Fields.MoneyLaunderingStatus
import FIX.Fields.MsgSeqNum
import FIX.Fields.MsgType
import FIX.Fields.MultiLegReportingType
import FIX.Fields.MultiLegRptTypeReq
import FIX.Fields.NearLegRatePrecision
import FIX.Fields.Nested2PartyID
import FIX.Fields.Nested2PartyIDSource
import FIX.Fields.Nested2PartyRole
import FIX.Fields.Nested2PartySubID
import FIX.Fields.Nested2PartySubIDType
import FIX.Fields.Nested3PartyID
import FIX.Fields.Nested3PartyIDSource
import FIX.Fields.Nested3PartyRole
import FIX.Fields.Nested3PartySubID
import FIX.Fields.Nested3PartySubIDType
import FIX.Fields.NestedPartyID
import FIX.Fields.NestedPartyIDSource
import FIX.Fields.NestedPartyRole
import FIX.Fields.NestedPartyRoleQualifier
import FIX.Fields.NestedPartySubID
import FIX.Fields.NestedPartySubIDType
import FIX.Fields.NetMoney
import FIX.Fields.NewSeqNo
import FIX.Fields.NoAllocs
import FIX.Fields.NoContAmts
import FIX.Fields.NoContraBrokers
import FIX.Fields.NoCustomFields
import FIX.Fields.NoEvents
import FIX.Fields.NoLegAllocs
import FIX.Fields.NoLegSecurityAltID
import FIX.Fields.NoLegStipulations
import FIX.Fields.NoLegs
import FIX.Fields.NoMiscFees
import FIX.Fields.NoNested2PartyIDs
import FIX.Fields.NoNested2PartySubIDs
import FIX.Fields.NoNested3PartyIDs
import FIX.Fields.NoNested3PartySubIDs
import FIX.Fields.NoNestedPartyIDs
import FIX.Fields.NoNestedPartySubIDs
import FIX.Fields.NoOrderAttributes
import FIX.Fields.NoPartyIDs
import FIX.Fields.NoPartySubIDs
import FIX.Fields.NoQuoteQualifiers
import FIX.Fields.NoRegulatoryTradeIDs
import FIX.Fields.NoRelatedSym
import FIX.Fields.NoRoutingIDs
import FIX.Fields.NoSecurityAltID
import FIX.Fields.NoStipulations
import FIX.Fields.NoTradingSessions
import FIX.Fields.NoTrdRegPublications
import FIX.Fields.NoUnderlyings
import FIX.Fields.NumDaysInterest
import FIX.Fields.OfferExAnteCost
import FIX.Fields.OfferExAnteCostPercentage
import FIX.Fields.OfferForwardPoints
import FIX.Fields.OfferForwardPoints2
import FIX.Fields.OfferInterestAtMaturity
import FIX.Fields.OfferPx
import FIX.Fields.OfferPx2
import FIX.Fields.OfferSize
import FIX.Fields.OfferSpotRate
import FIX.Fields.OfferYield
import FIX.Fields.OptAttribute
import FIX.Fields.OptionDate
import FIX.Fields.OptionPeriod
import FIX.Fields.OrdRejReason
import FIX.Fields.OrdStatus
import FIX.Fields.OrdStatusReqID
import FIX.Fields.OrdType
import FIX.Fields.OrderAttributeType
import FIX.Fields.OrderAttributeValue
import FIX.Fields.OrderCapacity
import FIX.Fields.OrderID
import FIX.Fields.OrderPercent
import FIX.Fields.OrderQty
import FIX.Fields.OrderQty2
import FIX.Fields.OrderRestrictions
import FIX.Fields.OrigClOrdID
import FIX.Fields.OrigCrossID
import FIX.Fields.OrigExecID
import FIX.Fields.OrigNotionalAmt
import FIX.Fields.PartialAllowed
import FIX.Fields.ParticipationRate
import FIX.Fields.PartyID
import FIX.Fields.PartyIDSource
import FIX.Fields.PartyRole
import FIX.Fields.PartyRoleQualifier
import FIX.Fields.PartySubID
import FIX.Fields.PartySubIDType
import FIX.Fields.Password
import FIX.Fields.PegLimitType
import FIX.Fields.PegMoveType
import FIX.Fields.PegOffsetType
import FIX.Fields.PegOffsetValue
import FIX.Fields.PegRoundDirection
import FIX.Fields.PegScope
import FIX.Fields.PeggedPrice
import FIX.Fields.Pool
import FIX.Fields.PositionEffect
import FIX.Fields.PreallocMethod
import FIX.Fields.PrevClosePx
import FIX.Fields.Price
import FIX.Fields.Price2
import FIX.Fields.PriceImprovement
import FIX.Fields.PriceType
import FIX.Fields.PriorityIndicator
import FIX.Fields.ProcessCode
import FIX.Fields.Product
import FIX.Fields.ProductType
import FIX.Fields.ProlongationCounter
import FIX.Fields.ProviderPersonStatus
import FIX.Fields.QtyType
import FIX.Fields.QuoteCancelType
import FIX.Fields.QuoteID
import FIX.Fields.QuotePriceType
import FIX.Fields.QuoteQualifier
import FIX.Fields.QuoteReqID
import FIX.Fields.QuoteRequestRejectReason
import FIX.Fields.QuoteRequestType
import FIX.Fields.QuoteRespID
import FIX.Fields.QuoteResponseLevel
import FIX.Fields.QuoteType
import FIX.Fields.RFQReqID
import FIX.Fields.RedemptionDate
import FIX.Fields.RefMsgType
import FIX.Fields.RefSeqNum
import FIX.Fields.RefSpotDate
import FIX.Fields.RefTagID
import FIX.Fields.RegistID
import FIX.Fields.RegulatoryLegRefID
import FIX.Fields.RegulatoryTradeID
import FIX.Fields.RegulatoryTradeIDSource
import FIX.Fields.RegulatoryTradeIDType
import FIX.Fields.RepoCollateralSecurityType
import FIX.Fields.ReportToExch
import FIX.Fields.ReportingParty
import FIX.Fields.RepurchaseRate
import FIX.Fields.RepurchaseTerm
import FIX.Fields.ResetSeqNumFlag
import FIX.Fields.RoundingDirection
import FIX.Fields.RoundingModulus
import FIX.Fields.RoutingID
import FIX.Fields.RoutingType
import FIX.Fields.SecondaryClOrdID
import FIX.Fields.SecondaryExecID
import FIX.Fields.SecondaryOrderID
import FIX.Fields.SecurityAltID
import FIX.Fields.SecurityAltIDSource
import FIX.Fields.SecurityDesc
import FIX.Fields.SecurityExchange
import FIX.Fields.SecurityID
import FIX.Fields.SecurityIDSource
import FIX.Fields.SecurityReqID
import FIX.Fields.SecurityRequestType
import FIX.Fields.SecurityResponseID
import FIX.Fields.SecurityResponseType
import FIX.Fields.SecuritySubType
import FIX.Fields.SecurityType
import FIX.Fields.SenderCompID
import FIX.Fields.SenderSubID
import FIX.Fields.SendingTime
import FIX.Fields.SessionRejectReason
import FIX.Fields.SettlCurrAmt
import FIX.Fields.SettlCurrBidFxRate
import FIX.Fields.SettlCurrFxRate
import FIX.Fields.SettlCurrFxRateCalc
import FIX.Fields.SettlCurrOfferFxRate
import FIX.Fields.SettlCurrency
import FIX.Fields.SettlDate
import FIX.Fields.SettlDate2
import FIX.Fields.SettlType
import FIX.Fields.Side
import FIX.Fields.SolicitedFlag
import FIX.Fields.SplitSettlDate
import FIX.Fields.SplitSettlDate2
import FIX.Fields.SplittingAllowed
import FIX.Fields.SpotRatePrecision
import FIX.Fields.Spread
import FIX.Fields.StartCash
import FIX.Fields.StartDate
import FIX.Fields.StateOrProvinceOfIssue
import FIX.Fields.StipulationType
import FIX.Fields.StipulationValue
import FIX.Fields.StopPx
import FIX.Fields.StrikeCurrency
import FIX.Fields.StrikePrice
import FIX.Fields.SwapDataRepository
import FIX.Fields.Symbol
import FIX.Fields.SymbolSfx
import FIX.Fields.TargetCompID
import FIX.Fields.TargetStrategy
import FIX.Fields.TargetStrategyParameters
import FIX.Fields.TargetStrategyPerformance
import FIX.Fields.TargetSubID
import FIX.Fields.TerminationType
import FIX.Fields.TestReqID
import FIX.Fields.Text
import FIX.Fields.TimeBracket
import FIX.Fields.TimeInForce
import FIX.Fields.TotNumReports
import FIX.Fields.TotalTakedown
import FIX.Fields.TradeDate
import FIX.Fields.TradeOriginationDate
import FIX.Fields.TradedFlatSwitch
import FIX.Fields.TradingSessionID
import FIX.Fields.TradingSessionSubID
import FIX.Fields.TransBkdTime
import FIX.Fields.TransactTime
import FIX.Fields.TrdRegPublicationReason
import FIX.Fields.TrdRegPublicationType
import FIX.Fields.TrdType
import FIX.Fields.UPICode
import FIX.Fields.UPICode2
import FIX.Fields.USIID
import FIX.Fields.USIID2
import FIX.Fields.USIPrefix
import FIX.Fields.USIPrefix2
import FIX.Fields.UTIID
import FIX.Fields.UTIID2
import FIX.Fields.UnderlyingLastPx
import FIX.Fields.UnderlyingLastQty
import FIX.Fields.UnderlyingMaturityDate
import FIX.Fields.UnderlyingSecurityDesc
import FIX.Fields.UnderlyingSecurityID
import FIX.Fields.UnderlyingSymbol
import FIX.Fields.ValidUntilTime
import FIX.Fields.WorkingIndicator
import FIX.Fields.Yield
import FIX.Fields.YieldCalcDate
import FIX.Fields.YieldRedemptionDate
import FIX.Fields.YieldRedemptionPrice
import FIX.Fields.YieldRedemptionPriceType
import FIX.Fields.YieldType
import Test.Syd

spec :: Spec
spec = do
  fieldSpec @Account
  fieldSpec @AvgPx
  fieldSpec @BeginSeqNo
  fieldSpec @BeginString
  fieldSpec @BodyLength
  fieldSpec @CheckSum
  fieldSpec @ClOrdID
  fieldSpec @Commission
  fieldSpec @CommType
  fieldSpec @CumQty
  fieldSpec @Currency
  fieldSpec @EndSeqNo
  fieldSpec @ExecID
  fieldSpec @ExecInst
  fieldSpec @ExecRefID
  fieldSpec @HandlInst
  fieldSpec @SecurityIDSource
  fieldSpec @IOIid
  fieldSpec @LastCapacity
  fieldSpec @LastMkt
  fieldSpec @LastPx
  fieldSpec @LastQty
  fieldSpec @LinesOfText
  fieldSpec @MsgSeqNum
  fieldSpec @MsgType
  fieldSpec @NewSeqNo
  fieldSpec @OrderID
  fieldSpec @OrderQty
  fieldSpec @OrdStatus
  fieldSpec @OrdType
  fieldSpec @OrigClOrdID
  fieldSpec @Price
  fieldSpec @RefSeqNum
  fieldSpec @SecurityID
  fieldSpec @SenderCompID
  fieldSpec @SenderSubID
  fieldSpec @SendingTime
  fieldSpec @Side
  fieldSpec @Symbol
  fieldSpec @TargetCompID
  fieldSpec @TargetSubID
  fieldSpec @Text
  fieldSpec @TimeInForce
  fieldSpec @TransactTime
  fieldSpec @ValidUntilTime
  fieldSpec @SettlType
  fieldSpec @SettlDate
  fieldSpec @SymbolSfx
  fieldSpec @ListID
  fieldSpec @AllocID
  fieldSpec @AvgPxPrecision
  fieldSpec @TradeDate
  fieldSpec @PositionEffect
  fieldSpec @NoAllocs
  fieldSpec @AllocAccount
  fieldSpec @AllocQty
  fieldSpec @ProcessCode
  fieldSpec @EncryptMethod
  fieldSpec @StopPx
  fieldSpec @ExDestination
  fieldSpec @OrdRejReason
  fieldSpec @Issuer
  fieldSpec @SecurityDesc
  fieldSpec @HeartBtInt
  fieldSpec @MinQty
  fieldSpec @MaxFloor
  fieldSpec @TestReqID
  fieldSpec @ReportToExch
  fieldSpec @LocateReqd
  fieldSpec @QuoteID
  fieldSpec @NetMoney
  fieldSpec @SettlCurrAmt
  fieldSpec @SettlCurrency
  fieldSpec @ForexReq
  fieldSpec @ExpireTime
  fieldSpec @QuoteReqID
  fieldSpec @BidPx
  fieldSpec @OfferPx
  fieldSpec @BidSize
  fieldSpec @OfferSize
  fieldSpec @NoMiscFees
  fieldSpec @MiscFeeAmt
  fieldSpec @MiscFeeCurr
  fieldSpec @MiscFeeType
  fieldSpec @PrevClosePx
  fieldSpec @ResetSeqNumFlag
  fieldSpec @NoRelatedSym
  fieldSpec @Headline
  fieldSpec @ExecType
  fieldSpec @LeavesQty
  fieldSpec @CashOrderQty
  fieldSpec @SettlCurrFxRate
  fieldSpec @SettlCurrFxRateCalc
  fieldSpec @NumDaysInterest
  fieldSpec @AccruedInterestRate
  fieldSpec @AccruedInterestAmt
  fieldSpec @SecurityType
  fieldSpec @EffectiveTime
  fieldSpec @BidSpotRate
  fieldSpec @BidForwardPoints
  fieldSpec @OfferSpotRate
  fieldSpec @OfferForwardPoints
  fieldSpec @OrderQty2
  fieldSpec @SettlDate2
  fieldSpec @LastSpotRate
  fieldSpec @LastForwardPoints
  fieldSpec @SecondaryOrderID
  fieldSpec @MaturityMonthYear
  fieldSpec @StrikePrice
  fieldSpec @CoveredOrUncovered
  fieldSpec @OptAttribute
  fieldSpec @SecurityExchange
  fieldSpec @MaxShow
  fieldSpec @PegOffsetValue
  fieldSpec @NoRoutingIDs
  fieldSpec @RoutingType
  fieldSpec @RoutingID
  fieldSpec @Spread
  fieldSpec @BenchmarkCurveCurrency
  fieldSpec @BenchmarkCurveName
  fieldSpec @BenchmarkCurvePoint
  fieldSpec @CouponRate
  fieldSpec @CouponPaymentDate
  fieldSpec @IssueDate
  fieldSpec @RepurchaseTerm
  fieldSpec @RepurchaseRate
  fieldSpec @Factor
  fieldSpec @TradeOriginationDate
  fieldSpec @ExDate
  fieldSpec @ContractMultiplier
  fieldSpec @NoStipulations
  fieldSpec @StipulationType
  fieldSpec @StipulationValue
  fieldSpec @YieldType
  fieldSpec @Yield
  fieldSpec @TotalTakedown
  fieldSpec @Concession
  fieldSpec @RepoCollateralSecurityType
  fieldSpec @RedemptionDate
  fieldSpec @LegCouponPaymentDate
  fieldSpec @LegIssueDate
  fieldSpec @LegRepoCollateralSecurityType
  fieldSpec @LegRepurchaseTerm
  fieldSpec @LegRepurchaseRate
  fieldSpec @LegFactor
  fieldSpec @LegRedemptionDate
  fieldSpec @CreditRating
  fieldSpec @LegCreditRating
  fieldSpec @TradedFlatSwitch
  fieldSpec @BasisFeatureDate
  fieldSpec @BasisFeaturePrice
  fieldSpec @QuoteCancelType
  fieldSpec @QuoteResponseLevel
  fieldSpec @QuoteRequestType
  fieldSpec @UnderlyingSecurityDesc
  fieldSpec @UnderlyingSecurityID
  fieldSpec @UnderlyingSymbol
  fieldSpec @SecurityReqID
  fieldSpec @SecurityRequestType
  fieldSpec @SecurityResponseID
  fieldSpec @SecurityResponseType
  fieldSpec @TradingSessionID
  fieldSpec @ContraTrader
  fieldSpec @EncodedIssuer
  fieldSpec @EncodedSecurityDesc
  fieldSpec @EncodedText
  fieldSpec @RefTagID
  fieldSpec @RefMsgType
  fieldSpec @SessionRejectReason
  fieldSpec @ContraBroker
  fieldSpec @ComplianceID
  fieldSpec @SolicitedFlag
  fieldSpec @ExecRestatementReason
  fieldSpec @BusinessRejectRefID
  fieldSpec @BusinessRejectReason
  fieldSpec @GrossTradeAmt
  fieldSpec @NoContraBrokers
  fieldSpec @NoTradingSessions
  fieldSpec @DiscretionInst
  fieldSpec @DiscretionOffsetValue
  fieldSpec @PriceType
  fieldSpec @DayOrderQty
  fieldSpec @DayCumQty
  fieldSpec @DayAvgPx
  fieldSpec @GTBookingInst
  fieldSpec @ExpireDate
  fieldSpec @ContraTradeQty
  fieldSpec @ContraTradeTime
  fieldSpec @MultiLegReportingType
  fieldSpec @PartyIDSource
  fieldSpec @PartyID
  fieldSpec @PartyRole
  fieldSpec @NoPartyIDs
  fieldSpec @NoSecurityAltID
  fieldSpec @SecurityAltID
  fieldSpec @SecurityAltIDSource
  fieldSpec @Product
  fieldSpec @CFICode
  fieldSpec @IndividualAllocID
  fieldSpec @RoundingDirection
  fieldSpec @RoundingModulus
  fieldSpec @CountryOfIssue
  fieldSpec @StateOrProvinceOfIssue
  fieldSpec @LocaleOfIssue
  fieldSpec @CommCurrency
  fieldSpec @CancellationRights
  fieldSpec @MoneyLaunderingStatus
  fieldSpec @TransBkdTime
  fieldSpec @ExecPriceType
  fieldSpec @ExecPriceAdjustment
  fieldSpec @Designation
  fieldSpec @FundRenewWaiv
  fieldSpec @RegistID
  fieldSpec @ExecValuationPoint
  fieldSpec @OrderPercent
  fieldSpec @NoContAmts
  fieldSpec @ContAmtType
  fieldSpec @ContAmtValue
  fieldSpec @ContAmtCurr
  fieldSpec @PartySubID
  fieldSpec @NestedPartyID
  fieldSpec @NestedPartyIDSource
  fieldSpec @SecondaryClOrdID
  fieldSpec @SecondaryExecID
  fieldSpec @OrderCapacity
  fieldSpec @OrderRestrictions
  fieldSpec @QuoteType
  fieldSpec @NestedPartyRole
  fieldSpec @NoNestedPartyIDs
  fieldSpec @MaturityDate
  fieldSpec @UnderlyingMaturityDate
  fieldSpec @InstrRegistry
  fieldSpec @CashMargin
  fieldSpec @NestedPartySubID
  fieldSpec @CrossID
  fieldSpec @CrossType
  fieldSpec @OrigCrossID
  fieldSpec @Password
  fieldSpec @NoLegs
  fieldSpec @LegCurrency
  fieldSpec @MultiLegRptTypeReq
  fieldSpec @LegPositionEffect
  fieldSpec @LegCoveredOrUncovered
  fieldSpec @LegPrice
  fieldSpec @AccountType
  fieldSpec @CustOrderCapacity
  fieldSpec @ClOrdLinkID
  fieldSpec @MassStatusReqID
  fieldSpec @LegSettlType
  fieldSpec @LegSettlDate
  fieldSpec @DayBookingInst
  fieldSpec @BookingUnit
  fieldSpec @PreallocMethod
  fieldSpec @LegCountryOfIssue
  fieldSpec @LegStateOrProvinceOfIssue
  fieldSpec @LegLocaleOfIssue
  fieldSpec @LegInstrRegistry
  fieldSpec @LegSymbol
  fieldSpec @LegSymbolSfx
  fieldSpec @LegSecurityID
  fieldSpec @LegSecurityIDSource
  fieldSpec @NoLegSecurityAltID
  fieldSpec @LegSecurityAltID
  fieldSpec @LegSecurityAltIDSource
  fieldSpec @LegProduct
  fieldSpec @LegCFICode
  fieldSpec @LegSecurityType
  fieldSpec @LegMaturityMonthYear
  fieldSpec @LegMaturityDate
  fieldSpec @LegStrikePrice
  fieldSpec @LegOptAttribute
  fieldSpec @LegContractMultiplier
  fieldSpec @LegCouponRate
  fieldSpec @LegSecurityExchange
  fieldSpec @LegIssuer
  fieldSpec @EncodedLegIssuer
  fieldSpec @LegSecurityDesc
  fieldSpec @EncodedLegSecurityDesc
  fieldSpec @LegRatioQty
  fieldSpec @LegSide
  fieldSpec @TradingSessionSubID
  fieldSpec @MidPx
  fieldSpec @BidYield
  fieldSpec @MidYield
  fieldSpec @OfferYield
  fieldSpec @ClearingFeeIndicator
  fieldSpec @WorkingIndicator
  fieldSpec @PriorityIndicator
  fieldSpec @PriceImprovement
  fieldSpec @Price2
  fieldSpec @LastForwardPoints2
  fieldSpec @BidForwardPoints2
  fieldSpec @OfferForwardPoints2
  fieldSpec @RFQReqID
  fieldSpec @MktBidPx
  fieldSpec @MktOfferPx
  fieldSpec @MinBidSize
  fieldSpec @MinOfferSize
  fieldSpec @UnderlyingLastPx
  fieldSpec @UnderlyingLastQty
  fieldSpec @LegRefID
  fieldSpec @ContraLegRefID
  fieldSpec @SettlCurrBidFxRate
  fieldSpec @SettlCurrOfferFxRate
  fieldSpec @QuoteRequestRejectReason
  fieldSpec @AcctIDSource
  fieldSpec @AllocAcctIDSource
  fieldSpec @BenchmarkPrice
  fieldSpec @BenchmarkPriceType
  fieldSpec @ContractSettlMonth
  fieldSpec @LastParPx
  fieldSpec @NoLegAllocs
  fieldSpec @LegAllocAccount
  fieldSpec @LegIndividualAllocID
  fieldSpec @LegAllocQty
  fieldSpec @LegAllocAcctIDSource
  fieldSpec @LegSettlCurrency
  fieldSpec @LegBenchmarkCurveCurrency
  fieldSpec @LegBenchmarkCurveName
  fieldSpec @LegBenchmarkCurvePoint
  fieldSpec @LegBenchmarkPrice
  fieldSpec @LegBenchmarkPriceType
  fieldSpec @LegBidPx
  fieldSpec @NoLegStipulations
  fieldSpec @LegOfferPx
  fieldSpec @LegPriceType
  fieldSpec @LegQty
  fieldSpec @LegStipulationType
  fieldSpec @LegStipulationValue
  fieldSpec @LegSwapType
  fieldSpec @Pool
  fieldSpec @QuotePriceType
  fieldSpec @QuoteRespID
  fieldSpec @QuoteQualifier
  fieldSpec @YieldRedemptionDate
  fieldSpec @YieldRedemptionPrice
  fieldSpec @YieldRedemptionPriceType
  fieldSpec @BenchmarkSecurityID
  fieldSpec @YieldCalcDate
  fieldSpec @NoUnderlyings
  fieldSpec @NoQuoteQualifiers
  fieldSpec @AllocSettlCurrency
  fieldSpec @InterestAtMaturity
  fieldSpec @LegDatedDate
  fieldSpec @LegPool
  fieldSpec @NoNested2PartyIDs
  fieldSpec @Nested2PartyID
  fieldSpec @Nested2PartyIDSource
  fieldSpec @Nested2PartyRole
  fieldSpec @Nested2PartySubID
  fieldSpec @BenchmarkSecurityIDSource
  fieldSpec @SecuritySubType
  fieldSpec @LegSecuritySubType
  fieldSpec @BookingType
  fieldSpec @TerminationType
  fieldSpec @OrdStatusReqID
  fieldSpec @CopyMsgIndicator
  fieldSpec @NoPartySubIDs
  fieldSpec @PartySubIDType
  fieldSpec @NoNestedPartySubIDs
  fieldSpec @NestedPartySubIDType
  fieldSpec @NoNested2PartySubIDs
  fieldSpec @Nested2PartySubIDType
  fieldSpec @TrdType
  fieldSpec @PegMoveType
  fieldSpec @PegOffsetType
  fieldSpec @PegLimitType
  fieldSpec @PegRoundDirection
  fieldSpec @PeggedPrice
  fieldSpec @PegScope
  fieldSpec @DiscretionMoveType
  fieldSpec @DiscretionOffsetType
  fieldSpec @DiscretionLimitType
  fieldSpec @DiscretionRoundDirection
  fieldSpec @DiscretionPrice
  fieldSpec @DiscretionScope
  fieldSpec @TargetStrategy
  fieldSpec @TargetStrategyParameters
  fieldSpec @ParticipationRate
  fieldSpec @TargetStrategyPerformance
  fieldSpec @LastLiquidityInd
  fieldSpec @QtyType
  fieldSpec @NoEvents
  fieldSpec @EventType
  fieldSpec @EventDate
  fieldSpec @EventPx
  fieldSpec @EventText
  fieldSpec @DatedDate
  fieldSpec @InterestAccrualDate
  fieldSpec @CPProgram
  fieldSpec @CPRegType
  fieldSpec @MiscFeeBasis
  fieldSpec @MarginRatio
  fieldSpec @TotNumReports
  fieldSpec @LastRptRequested
  fieldSpec @AgreementDesc
  fieldSpec @AgreementID
  fieldSpec @AgreementDate
  fieldSpec @StartDate
  fieldSpec @EndDate
  fieldSpec @AgreementCurrency
  fieldSpec @DeliveryType
  fieldSpec @EndAccruedInterestAmt
  fieldSpec @StartCash
  fieldSpec @EndCash
  fieldSpec @LegStrikeCurrency
  fieldSpec @TimeBracket
  fieldSpec @StrikeCurrency
  fieldSpec @NoNested3PartyIDs
  fieldSpec @Nested3PartyID
  fieldSpec @Nested3PartyIDSource
  fieldSpec @Nested3PartyRole
  fieldSpec @NoNested3PartySubIDs
  fieldSpec @Nested3PartySubID
  fieldSpec @Nested3PartySubIDType
  fieldSpec @LegContractSettlMonth
  fieldSpec @LegInterestAccrualDate
  fieldSpec @CstmApplVerID
  fieldSpec @NoRegulatoryTradeIDs
  fieldSpec @RegulatoryTradeID
  fieldSpec @RegulatoryTradeIDSource
  fieldSpec @RegulatoryTradeIDType
  fieldSpec @PartyRoleQualifier
  fieldSpec @NestedPartyRoleQualifier
  fieldSpec @RegulatoryLegRefID
  fieldSpec @NoOrderAttributes
  fieldSpec @OrderAttributeType
  fieldSpec @OrderAttributeValue
  fieldSpec @NoTrdRegPublications
  fieldSpec @TrdRegPublicationType
  fieldSpec @TrdRegPublicationReason
  fieldSpec @UPICode
  fieldSpec @LegUPICode
  fieldSpec @BidPx2
  fieldSpec @OfferPx2
  fieldSpec @LastPx2
  fieldSpec @RefSpotDate
  fieldSpec @ProductType
  fieldSpec @DayCount
  fieldSpec @Fiduciary
  fieldSpec @FixingReference
  fieldSpec @SplittingAllowed
  fieldSpec @PartialAllowed
  fieldSpec @BidInterestAtMaturity
  fieldSpec @OfferInterestAtMaturity
  fieldSpec @MaturityDate2
  fieldSpec @FixingCode
  fieldSpec @FixingDate
  fieldSpec @FixingTime
  fieldSpec @NoCustomFields
  fieldSpec @CustomFieldsName
  fieldSpec @CustomFieldsValue
  fieldSpec @CustomFieldsContext
  fieldSpec @InterestSettlType
  fieldSpec @ProlongationCounter
  fieldSpec @OrigNotionalAmt
  fieldSpec @OrigExecID
  fieldSpec @USIPrefix
  fieldSpec @USIID
  fieldSpec @USIPrefix2
  fieldSpec @USIID2
  fieldSpec @ExecutionVenueType
  fieldSpec @ReportingParty
  fieldSpec @SwapDataRepository
  fieldSpec @ProviderPersonStatus
  fieldSpec @MidSpotRate
  fieldSpec @MidPx2
  fieldSpec @LegMidPx
  fieldSpec @UTIID
  fieldSpec @UTIID2
  fieldSpec @UPICode2
  fieldSpec @OptionPeriod
  fieldSpec @OptionDate
  fieldSpec @BidExAnteCost
  fieldSpec @OfferExAnteCost
  fieldSpec @BidExAnteCostPercentage
  fieldSpec @OfferExAnteCostPercentage
  fieldSpec @LegBidExAnteCost
  fieldSpec @LegOfferExAnteCost
  fieldSpec @LegBidExAnteCostPercentage
  fieldSpec @LegOfferExAnteCostPercentage
  fieldSpec @SpotRatePrecision
  fieldSpec @ForwardRatePrecision
  fieldSpec @ForwardPointsPrecision
  fieldSpec @InterestRatePrecision
  fieldSpec @NearLegRatePrecision
  fieldSpec @FarLegRatePrecision
  fieldSpec @SplitSettlDate
  fieldSpec @SplitSettlDate2
  fieldSpec @LegSplitSettlDate
