{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Components.Gen where

import Data.GenValidity
import Data.GenValidity.ByteString ()
import FIX.Fields.Gen ()
import FIX.Groups.AllocsGroupElem
import FIX.Groups.CustomFieldsGroupElem
import FIX.Groups.LinesOfTextGroupElem
import FIX.Groups.NestedPartyIDsGroupElem
import FIX.Groups.OrderAttributeGroupElem
import FIX.Groups.PartyIDsGroupElem
import FIX.Groups.RegulatoryTradeIDGroupElem
import FIX.Groups.RelatedSymGroupElem
import FIX.Groups.RoutingIDsGroupElem

instance GenValid AllocsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid CustomFieldsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid LinesOfTextGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid NestedPartyIDsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid OrderAttributeGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid PartyIDsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid RegulatoryTradeIDGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid RelatedSymGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid RoutingIDsGroupElem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
