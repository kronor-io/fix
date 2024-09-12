{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages.QuoteRequestReject where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.MsgType
import FIX.Fields.QuoteReqID
import FIX.Fields.QuoteRequestRejectReason
import FIX.Fields.Text
import FIX.Groups.Class
import FIX.Groups.QuotReqRjctGrpGroupElem
import FIX.Messages.Class
import GHC.Generics (Generic)

-- | MessageSpec
--   { messageName = "QuoteRequestReject"
--   , messageType = "AG"
--   , messageCategory = "app"
--   , messagePieces =
--       [ MessagePieceField "QuoteReqID" True
--       , MessagePieceField "QuoteRequestRejectReason" True
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "QuotReqRjctGrp"
--             , groupNumberField = "NoRelatedSym"
--             , groupPieces =
--                 [ MessagePieceField "Symbol" True
--                 , MessagePieceField "QuoteType" True
--                 , MessagePieceField "Currency" False
--                 ]
--             }
--           True
--       , MessagePieceField "Text" True
--       ]
--   }
data QuoteRequestReject = QuoteRequestReject
  { quoteRequestRejectQuoteReqID :: !QuoteReqID,
    quoteRequestRejectQuoteRequestRejectReason :: !QuoteRequestRejectReason,
    quoteRequestRejectQuotReqRjctGrpGroup :: !(NonEmpty QuotReqRjctGrpGroupElem),
    quoteRequestRejectText :: !Text
  }
  deriving stock (Show, Eq, Generic)

instance Validity QuoteRequestReject

instance IsComponent QuoteRequestReject where
  toComponentFields ((QuoteRequestReject {..})) =
    mconcat
      [ requiredFieldB quoteRequestRejectQuoteReqID,
        requiredFieldB quoteRequestRejectQuoteRequestRejectReason,
        requiredGroupB quoteRequestRejectQuotReqRjctGrpGroup,
        requiredFieldB quoteRequestRejectText
      ]
  fromComponentFields = do
    quoteRequestRejectQuoteReqID <- requiredFieldP
    quoteRequestRejectQuoteRequestRejectReason <- requiredFieldP
    quoteRequestRejectQuotReqRjctGrpGroup <- requiredGroupP
    quoteRequestRejectText <- requiredFieldP
    pure (QuoteRequestReject {..})

instance IsMessage QuoteRequestReject where
  messageType Proxy = MsgTypeQuoteRequestReject

makeQuoteRequestReject :: QuoteReqID -> (QuoteRequestRejectReason -> (NonEmpty QuotReqRjctGrpGroupElem -> (Text -> QuoteRequestReject)))
makeQuoteRequestReject quoteRequestRejectQuoteReqID quoteRequestRejectQuoteRequestRejectReason quoteRequestRejectQuotReqRjctGrpGroup quoteRequestRejectText =
  let
   in (QuoteRequestReject {..})