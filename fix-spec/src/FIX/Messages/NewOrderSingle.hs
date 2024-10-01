{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages.NewOrderSingle where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.Account
import FIX.Fields.ClOrdID
import FIX.Fields.Currency
import FIX.Fields.DayCount
import FIX.Fields.FixingReference
import FIX.Fields.Issuer
import FIX.Fields.MaturityDate
import FIX.Fields.MaturityDate2
import FIX.Fields.MsgType
import FIX.Fields.OptionDate
import FIX.Fields.OrdType
import FIX.Fields.OrderQty
import FIX.Fields.Price
import FIX.Fields.ProductType
import FIX.Fields.QuoteID
import FIX.Fields.SettlDate
import FIX.Fields.SettlDate2
import FIX.Fields.Side
import FIX.Fields.Symbol
import FIX.Fields.TransactTime
import FIX.Groups.Class
import FIX.Messages.Class
import GHC.Generics (Generic)

-- | MessageSpec
--   { messageName = "NewOrderSingle"
--   , messageType = "D"
--   , messageCategory = "app"
--   , messagePieces =
--       [ MessagePieceField "ClOrdID" True
--       , MessagePieceField "Account" True
--       , MessagePieceField "SettlDate" True
--       , MessagePieceField "Symbol" True
--       , MessagePieceField "MaturityDate" False
--       , MessagePieceField "MaturityDate2" False
--       , MessagePieceField "Issuer" False
--       , MessagePieceField "Side" True
--       , MessagePieceField "TransactTime" True
--       , MessagePieceField "OrderQty" True
--       , MessagePieceField "OrdType" True
--       , MessagePieceField "Price" False
--       , MessagePieceField "Currency" True
--       , MessagePieceField "QuoteID" True
--       , MessagePieceField "SettlDate2" False
--       , MessagePieceField "ProductType" False
--       , MessagePieceField "DayCount" False
--       , MessagePieceField "FixingReference" False
--       , MessagePieceField "OptionDate" False
--       ]
--   }
data NewOrderSingle = NewOrderSingle
  { newOrderSingleClOrdID :: !ClOrdID,
    newOrderSingleAccount :: !Account,
    newOrderSingleSettlDate :: !SettlDate,
    newOrderSingleSymbol :: !Symbol,
    newOrderSingleMaturityDate :: !(Maybe MaturityDate),
    newOrderSingleMaturityDate2 :: !(Maybe MaturityDate2),
    newOrderSingleIssuer :: !(Maybe Issuer),
    newOrderSingleSide :: !Side,
    newOrderSingleTransactTime :: !TransactTime,
    newOrderSingleOrderQty :: !OrderQty,
    newOrderSingleOrdType :: !OrdType,
    newOrderSinglePrice :: !(Maybe Price),
    newOrderSingleCurrency :: !Currency,
    newOrderSingleQuoteID :: !QuoteID,
    newOrderSingleSettlDate2 :: !(Maybe SettlDate2),
    newOrderSingleProductType :: !(Maybe ProductType),
    newOrderSingleDayCount :: !(Maybe DayCount),
    newOrderSingleFixingReference :: !(Maybe FixingReference),
    newOrderSingleOptionDate :: !(Maybe OptionDate)
  }
  deriving stock (Show, Eq, Generic)

instance Validity NewOrderSingle

instance IsComponent NewOrderSingle where
  toComponentFields ((NewOrderSingle {..})) =
    mconcat
      [ requiredFieldB newOrderSingleClOrdID,
        requiredFieldB newOrderSingleAccount,
        requiredFieldB newOrderSingleSettlDate,
        requiredFieldB newOrderSingleSymbol,
        optionalFieldB newOrderSingleMaturityDate,
        optionalFieldB newOrderSingleMaturityDate2,
        optionalFieldB newOrderSingleIssuer,
        requiredFieldB newOrderSingleSide,
        requiredFieldB newOrderSingleTransactTime,
        requiredFieldB newOrderSingleOrderQty,
        requiredFieldB newOrderSingleOrdType,
        optionalFieldB newOrderSinglePrice,
        requiredFieldB newOrderSingleCurrency,
        requiredFieldB newOrderSingleQuoteID,
        optionalFieldB newOrderSingleSettlDate2,
        optionalFieldB newOrderSingleProductType,
        optionalFieldB newOrderSingleDayCount,
        optionalFieldB newOrderSingleFixingReference,
        optionalFieldB newOrderSingleOptionDate
      ]
  fromComponentFields = do
    newOrderSingleClOrdID <- requiredFieldP
    newOrderSingleAccount <- requiredFieldP
    newOrderSingleSettlDate <- requiredFieldP
    newOrderSingleSymbol <- requiredFieldP
    newOrderSingleMaturityDate <- optionalFieldP
    newOrderSingleMaturityDate2 <- optionalFieldP
    newOrderSingleIssuer <- optionalFieldP
    newOrderSingleSide <- requiredFieldP
    newOrderSingleTransactTime <- requiredFieldP
    newOrderSingleOrderQty <- requiredFieldP
    newOrderSingleOrdType <- requiredFieldP
    newOrderSinglePrice <- optionalFieldP
    newOrderSingleCurrency <- requiredFieldP
    newOrderSingleQuoteID <- requiredFieldP
    newOrderSingleSettlDate2 <- optionalFieldP
    newOrderSingleProductType <- optionalFieldP
    newOrderSingleDayCount <- optionalFieldP
    newOrderSingleFixingReference <- optionalFieldP
    newOrderSingleOptionDate <- optionalFieldP
    pure (NewOrderSingle {..})

instance IsMessage NewOrderSingle where
  messageType Proxy = MsgTypeNewOrderSingle

makeNewOrderSingle :: ClOrdID -> (Account -> (SettlDate -> (Symbol -> (Side -> (TransactTime -> (OrderQty -> (OrdType -> (Currency -> (QuoteID -> NewOrderSingle)))))))))
makeNewOrderSingle newOrderSingleClOrdID newOrderSingleAccount newOrderSingleSettlDate newOrderSingleSymbol newOrderSingleSide newOrderSingleTransactTime newOrderSingleOrderQty newOrderSingleOrdType newOrderSingleCurrency newOrderSingleQuoteID =
  let newOrderSingleMaturityDate = Nothing
      newOrderSingleMaturityDate2 = Nothing
      newOrderSingleIssuer = Nothing
      newOrderSinglePrice = Nothing
      newOrderSingleSettlDate2 = Nothing
      newOrderSingleProductType = Nothing
      newOrderSingleDayCount = Nothing
      newOrderSingleFixingReference = Nothing
      newOrderSingleOptionDate = Nothing
   in (NewOrderSingle {..})
