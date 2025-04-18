{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.QuoteRequestRelatedSymQuoteQualifiersGroupElem where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.MsgType
import FIX.Fields.NoQuoteQualifiers
import FIX.Fields.QuoteQualifier
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "QuoteRequestRelatedSymQuoteQualifiers"
--   , groupNumberField = "NoQuoteQualifiers"
--   , groupPieces = [ MessagePieceField "QuoteQualifier" True ]
--   }
data QuoteRequestRelatedSymQuoteQualifiersGroupElem = QuoteRequestRelatedSymQuoteQualifiersGroupElem {quoteRequestRelatedSymQuoteQualifiersGroupElemQuoteQualifier :: !QuoteQualifier}
  deriving stock (Show, Eq, Generic)

instance Validity QuoteRequestRelatedSymQuoteQualifiersGroupElem

instance IsComponent QuoteRequestRelatedSymQuoteQualifiersGroupElem where
  toComponentFields ((QuoteRequestRelatedSymQuoteQualifiersGroupElem {..})) = mconcat [requiredFieldB quoteRequestRelatedSymQuoteQualifiersGroupElemQuoteQualifier]
  fromComponentFields = do
    quoteRequestRelatedSymQuoteQualifiersGroupElemQuoteQualifier <- requiredFieldP
    pure (QuoteRequestRelatedSymQuoteQualifiersGroupElem {..})

instance IsGroupElement QuoteRequestRelatedSymQuoteQualifiersGroupElem where
  type GroupNumField QuoteRequestRelatedSymQuoteQualifiersGroupElem = NoQuoteQualifiers
  mkGroupNum Proxy = NoQuoteQualifiers
  countGroupNum Proxy = unNoQuoteQualifiers

makeQuoteRequestRelatedSymQuoteQualifiersGroupElem :: QuoteQualifier -> QuoteRequestRelatedSymQuoteQualifiersGroupElem
makeQuoteRequestRelatedSymQuoteQualifiersGroupElem quoteRequestRelatedSymQuoteQualifiersGroupElemQuoteQualifier =
  let
   in (QuoteRequestRelatedSymQuoteQualifiersGroupElem {..})
