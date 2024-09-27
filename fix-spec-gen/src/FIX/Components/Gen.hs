{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Components.Gen where

import Data.GenValidity
import Data.GenValidity.ByteString ()
import FIX.Components.ExAnteData
import FIX.Components.FinancingDetails
import FIX.Components.Instrument
import FIX.Components.InstrumentExtension
import FIX.Components.InstrumentLeg
import FIX.Components.LegBenchmarkCurveData
import FIX.Components.LegExAnteData
import FIX.Components.OrderQtyData
import FIX.Components.SpreadOrBenchmarkCurveData
import FIX.Components.UnderlyingInstrument
import FIX.Components.YieldData
import FIX.Fields.Gen ()
import FIX.Groups.InstrumentEventsGroupElem
import FIX.Groups.InstrumentExtensionInstrAttribGroupElem
import FIX.Groups.InstrumentLegLegSecurityAltIDGroupElem
import FIX.Groups.InstrumentSecurityAltIDGroupElem
import FIX.Groups.NewsLinesOfTextGroupElem
import FIX.Groups.NewsRoutingIDsGroupElem
import FIX.Groups.QuoteCancelPartiesGroupElem
import FIX.Groups.QuoteCancelPartiesPartySubIDsGroupElem
import FIX.Groups.QuoteCancelQuoteEntriesGroupElem
import FIX.Groups.QuoteCancelQuoteEntriesLegsGroupElem
import FIX.Groups.QuoteCancelQuoteEntriesUnderlyingsGroupElem
import FIX.Groups.QuoteLegsGroupElem
import FIX.Groups.QuoteLegsLegStipulationsGroupElem
import FIX.Groups.QuoteLegsNestedPartiesGroupElem
import FIX.Groups.QuoteLegsNestedPartiesNestedPartySubIDsGroupElem
import FIX.Groups.QuotePartiesGroupElem
import FIX.Groups.QuotePartiesPartySubIDsGroupElem
import FIX.Groups.QuoteQuoteQualifiersGroupElem
import FIX.Groups.QuoteRequestCustomFieldsGroupElem
import FIX.Groups.QuoteRequestOrderAttributesGroupElem
import FIX.Groups.QuoteRequestPartiesGroupElem
import FIX.Groups.QuoteRequestPartiesPartySubIDsGroupElem
import FIX.Groups.QuoteRequestRegulatoryTradeIDsGroupElem
import FIX.Groups.QuoteRequestRejectPartiesGroupElem
import FIX.Groups.QuoteRequestRejectPartiesPartySubIDsGroupElem
import FIX.Groups.QuoteRequestRejectQuoteQualifiersGroupElem
import FIX.Groups.QuoteRequestRejectRelatedSymGroupElem
import FIX.Groups.QuoteRequestRejectRelatedSymLegsGroupElem
import FIX.Groups.QuoteRequestRejectRelatedSymLegsLegStipulationsGroupElem
import FIX.Groups.QuoteRequestRejectRelatedSymLegsNestedPartiesGroupElem
import FIX.Groups.QuoteRequestRejectRelatedSymLegsNestedPartiesNestedPartySubIDsGroupElem
import FIX.Groups.QuoteRequestRejectRelatedSymStipulationsGroupElem
import FIX.Groups.QuoteRequestRejectRelatedSymUnderlyingsGroupElem
import FIX.Groups.QuoteRequestRelatedSymAllocsGroupElem
import FIX.Groups.QuoteRequestRelatedSymGroupElem
import FIX.Groups.QuoteRequestRelatedSymLegsGroupElem
import FIX.Groups.QuoteRequestRelatedSymLegsLegAllocsGroupElem
import FIX.Groups.QuoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElem
import FIX.Groups.QuoteRequestRelatedSymLegsLegAllocsNestedPartiesNestedPartySubIDsGroupElem
import FIX.Groups.QuoteRequestRelatedSymQuoteQualifiersGroupElem
import FIX.Groups.QuoteRequestRelatedSymStipulationsGroupElem
import FIX.Groups.QuoteRequestRelatedSymUnderlyingsGroupElem
import FIX.Groups.QuoteStipulationsGroupElem
import FIX.Groups.QuoteUnderlyingsGroupElem
import FIX.Groups.SecurityDefinitionLegsGroupElem
import FIX.Groups.SecurityDefinitionRequestLegsGroupElem
import FIX.Groups.SecurityDefinitionRequestUnderlyingsGroupElem
import FIX.Groups.SecurityDefinitionUnderlyingsGroupElem
import FIX.Groups.UnderlyingInstrumentUnderlyingSecurityAltIDGroupElem
import FIX.Groups.UnderlyingInstrumentUnderlyingStipulationsGroupElem

instance GenValid InstrumentEventsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid InstrumentExtensionInstrAttribGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid InstrumentLegLegSecurityAltIDGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid InstrumentSecurityAltIDGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid NewsLinesOfTextGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid NewsRoutingIDsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteCancelPartiesGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteCancelPartiesPartySubIDsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteCancelQuoteEntriesGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteCancelQuoteEntriesLegsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteCancelQuoteEntriesUnderlyingsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteLegsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteLegsLegStipulationsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteLegsNestedPartiesGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteLegsNestedPartiesNestedPartySubIDsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuotePartiesGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuotePartiesPartySubIDsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteQuoteQualifiersGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestCustomFieldsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestOrderAttributesGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestPartiesGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestPartiesPartySubIDsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestRegulatoryTradeIDsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestRejectPartiesGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestRejectPartiesPartySubIDsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestRejectQuoteQualifiersGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestRejectRelatedSymGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestRejectRelatedSymLegsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestRejectRelatedSymLegsLegStipulationsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestRejectRelatedSymLegsNestedPartiesGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestRejectRelatedSymLegsNestedPartiesNestedPartySubIDsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestRejectRelatedSymStipulationsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestRejectRelatedSymUnderlyingsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestRelatedSymGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestRelatedSymAllocsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestRelatedSymLegsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestRelatedSymLegsLegAllocsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestRelatedSymLegsLegAllocsNestedPartiesGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestRelatedSymLegsLegAllocsNestedPartiesNestedPartySubIDsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestRelatedSymQuoteQualifiersGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestRelatedSymStipulationsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteRequestRelatedSymUnderlyingsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteStipulationsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QuoteUnderlyingsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid SecurityDefinitionLegsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid SecurityDefinitionRequestLegsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid SecurityDefinitionRequestUnderlyingsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid SecurityDefinitionUnderlyingsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid UnderlyingInstrumentUnderlyingSecurityAltIDGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid UnderlyingInstrumentUnderlyingStipulationsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Instrument where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid UnderlyingInstrument where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid InstrumentLeg where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid InstrumentExtension where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid OrderQtyData where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid SpreadOrBenchmarkCurveData where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid LegBenchmarkCurveData where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid YieldData where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid FinancingDetails where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ExAnteData where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid LegExAnteData where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
