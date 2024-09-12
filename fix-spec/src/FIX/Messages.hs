{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages
  ( AnyMessage (..),
    anyMessageType,
    anyMessageB,
    anyMessageP,
    IsAnyMessage (..),
    module X,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as ByteString
import Data.Validity
import Data.Void (Void)
import FIX.Fields
import FIX.Messages.Class
import FIX.Messages.Envelope
import FIX.Messages.Heartbeat as X
import FIX.Messages.Logon as X
import FIX.Messages.Logout as X
import FIX.Messages.NewOrderSingle as X
import FIX.Messages.News as X
import FIX.Messages.Quote as X
import FIX.Messages.QuoteCancel as X
import FIX.Messages.QuoteRequest as X
import FIX.Messages.QuoteRequestReject as X
import FIX.Messages.Reject as X
import GHC.Generics (Generic)
import Text.Megaparsec

data AnyMessage
  = SomeLogon !Logon
  | SomeHeartbeat !Heartbeat
  | SomeReject !Reject
  | SomeLogout !Logout
  | SomeNews !News
  | SomeQuoteRequest !QuoteRequest
  | SomeQuoteRequestReject !QuoteRequestReject
  | SomeQuote !Quote
  | SomeQuoteCancel !QuoteCancel
  | SomeNewOrderSingle !NewOrderSingle
  deriving stock (Show, Eq, Generic)

instance Validity AnyMessage

anyMessageType :: AnyMessage -> MsgType
anyMessageType = \case
  SomeLogon _ -> MsgTypeLogon
  SomeHeartbeat _ -> MsgTypeHeartbeat
  SomeReject _ -> MsgTypeReject
  SomeLogout _ -> MsgTypeLogout
  SomeNews _ -> MsgTypeNews
  SomeQuoteRequest _ -> MsgTypeQuoteRequest
  SomeQuoteRequestReject _ -> MsgTypeQuoteRequestReject
  SomeQuote _ -> MsgTypeQuote
  SomeQuoteCancel _ -> MsgTypeQuoteCancel
  SomeNewOrderSingle _ -> MsgTypeNewOrderSingle

anyMessageB :: Envelope AnyMessage -> ByteString.Builder
anyMessageB ((Envelope {..})) =
  let mb ::
        forall a.
        (IsMessage a) =>
        a ->
        ByteString.Builder
      mb = messageB envelopeHeader envelopeTrailer
   in case envelopeContents of
        SomeLogon f -> mb f
        SomeHeartbeat f -> mb f
        SomeReject f -> mb f
        SomeLogout f -> mb f
        SomeNews f -> mb f
        SomeQuoteRequest f -> mb f
        SomeQuoteRequestReject f -> mb f
        SomeQuote f -> mb f
        SomeQuoteCancel f -> mb f
        SomeNewOrderSingle f -> mb f

anyMessageP :: Parsec Void ByteString (Envelope AnyMessage)
anyMessageP = do
  SomeBeginString bs <- anyFieldP
  SomeBodyLength bl <- anyFieldP
  SomeMsgType typ <- anyFieldP
  let mp ::
        forall f.
        (IsMessage f) =>
        Parsec Void ByteString (Envelope f)
      mp = messageP bs bl typ
  case typ of
    MsgTypeLogon -> fmap SomeLogon <$> mp
    MsgTypeHeartbeat -> fmap SomeHeartbeat <$> mp
    MsgTypeReject -> fmap SomeReject <$> mp
    MsgTypeLogout -> fmap SomeLogout <$> mp
    MsgTypeNews -> fmap SomeNews <$> mp
    MsgTypeQuoteRequest -> fmap SomeQuoteRequest <$> mp
    MsgTypeQuoteRequestReject -> fmap SomeQuoteRequestReject <$> mp
    MsgTypeQuote -> fmap SomeQuote <$> mp
    MsgTypeQuoteCancel -> fmap SomeQuoteCancel <$> mp
    MsgTypeNewOrderSingle -> fmap SomeNewOrderSingle <$> mp
    _ -> fail ("Unknown message tag: " <> show typ)

class (IsMessage a) => IsAnyMessage a where
  unpackAnyMessage :: AnyMessage -> Maybe a
  packAnyMessage :: a -> AnyMessage

instance IsAnyMessage Logon where
  packAnyMessage = SomeLogon
  unpackAnyMessage = \case
    SomeLogon f -> Just f
    _ -> Nothing

instance IsAnyMessage Heartbeat where
  packAnyMessage = SomeHeartbeat
  unpackAnyMessage = \case
    SomeHeartbeat f -> Just f
    _ -> Nothing

instance IsAnyMessage Reject where
  packAnyMessage = SomeReject
  unpackAnyMessage = \case
    SomeReject f -> Just f
    _ -> Nothing

instance IsAnyMessage Logout where
  packAnyMessage = SomeLogout
  unpackAnyMessage = \case
    SomeLogout f -> Just f
    _ -> Nothing

instance IsAnyMessage News where
  packAnyMessage = SomeNews
  unpackAnyMessage = \case
    SomeNews f -> Just f
    _ -> Nothing

instance IsAnyMessage QuoteRequest where
  packAnyMessage = SomeQuoteRequest
  unpackAnyMessage = \case
    SomeQuoteRequest f -> Just f
    _ -> Nothing

instance IsAnyMessage QuoteRequestReject where
  packAnyMessage = SomeQuoteRequestReject
  unpackAnyMessage = \case
    SomeQuoteRequestReject f -> Just f
    _ -> Nothing

instance IsAnyMessage Quote where
  packAnyMessage = SomeQuote
  unpackAnyMessage = \case
    SomeQuote f -> Just f
    _ -> Nothing

instance IsAnyMessage QuoteCancel where
  packAnyMessage = SomeQuoteCancel
  unpackAnyMessage = \case
    SomeQuoteCancel f -> Just f
    _ -> Nothing

instance IsAnyMessage NewOrderSingle where
  packAnyMessage = SomeNewOrderSingle
  unpackAnyMessage = \case
    SomeNewOrderSingle f -> Just f
    _ -> Nothing
