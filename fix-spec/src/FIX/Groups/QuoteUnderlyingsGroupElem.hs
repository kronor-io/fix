{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.QuoteUnderlyingsGroupElem where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Components.UnderlyingInstrument
import FIX.Fields.MsgType
import FIX.Fields.NoUnderlyings
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "QuoteUnderlyings"
--   , groupNumberField = "NoUnderlyings"
--   , groupPieces =
--       [ MessagePieceComponent "UnderlyingInstrument" True ]
--   }
data QuoteUnderlyingsGroupElem = QuoteUnderlyingsGroupElem {quoteUnderlyingsGroupElemUnderlyingInstrument :: !UnderlyingInstrument}
  deriving stock (Show, Eq, Generic)

instance Validity QuoteUnderlyingsGroupElem

instance IsComponent QuoteUnderlyingsGroupElem where
  toComponentFields ((QuoteUnderlyingsGroupElem {..})) = mconcat [requiredComponentB quoteUnderlyingsGroupElemUnderlyingInstrument]
  fromComponentFields = do
    quoteUnderlyingsGroupElemUnderlyingInstrument <- requiredComponentP
    pure (QuoteUnderlyingsGroupElem {..})

instance IsGroupElement QuoteUnderlyingsGroupElem where
  type GroupNumField QuoteUnderlyingsGroupElem = NoUnderlyings
  mkGroupNum Proxy = NoUnderlyings
  countGroupNum Proxy = unNoUnderlyings

makeQuoteUnderlyingsGroupElem :: UnderlyingInstrument -> QuoteUnderlyingsGroupElem
makeQuoteUnderlyingsGroupElem quoteUnderlyingsGroupElemUnderlyingInstrument =
  let
   in (QuoteUnderlyingsGroupElem {..})
