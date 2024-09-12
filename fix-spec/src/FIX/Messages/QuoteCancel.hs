{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages.QuoteCancel where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.MsgType
import FIX.Fields.ProductType
import FIX.Fields.QuoteCancelType
import FIX.Fields.QuoteID
import FIX.Fields.QuoteReqID
import FIX.Groups.Class
import FIX.Messages.Class
import GHC.Generics (Generic)

-- | MessageSpec
--   { messageName = "QuoteCancel"
--   , messageType = "Z"
--   , messageCategory = "app"
--   , messagePieces =
--       [ MessagePieceField "QuoteID" True
--       , MessagePieceField "QuoteReqID" True
--       , MessagePieceField "QuoteCancelType" True
--       , MessagePieceField "ProductType" True
--       ]
--   }
data QuoteCancel = QuoteCancel
  { quoteCancelQuoteID :: !QuoteID,
    quoteCancelQuoteReqID :: !QuoteReqID,
    quoteCancelQuoteCancelType :: !QuoteCancelType,
    quoteCancelProductType :: !ProductType
  }
  deriving stock (Show, Eq, Generic)

instance Validity QuoteCancel

instance IsComponent QuoteCancel where
  toComponentFields ((QuoteCancel {..})) =
    mconcat
      [ requiredFieldB quoteCancelQuoteID,
        requiredFieldB quoteCancelQuoteReqID,
        requiredFieldB quoteCancelQuoteCancelType,
        requiredFieldB quoteCancelProductType
      ]
  fromComponentFields = do
    quoteCancelQuoteID <- requiredFieldP
    quoteCancelQuoteReqID <- requiredFieldP
    quoteCancelQuoteCancelType <- requiredFieldP
    quoteCancelProductType <- requiredFieldP
    pure (QuoteCancel {..})

instance IsMessage QuoteCancel where
  messageType Proxy = MsgTypeQuoteCancel

makeQuoteCancel :: QuoteID -> (QuoteReqID -> (QuoteCancelType -> (ProductType -> QuoteCancel)))
makeQuoteCancel quoteCancelQuoteID quoteCancelQuoteReqID quoteCancelQuoteCancelType quoteCancelProductType =
  let
   in (QuoteCancel {..})
