{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages
  ( AnyMessage (..),
    anyMessageP,
    anyMessageB,
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
import GHC.Generics (Generic)
import Text.Megaparsec

data AnyMessage
  = SomeHeartbeat !Heartbeat
  | SomeLogon !Logon
  deriving stock (Show, Eq, Generic)

instance Validity AnyMessage

anyMessageB :: Envelope AnyMessage -> ByteString.Builder
anyMessageB ((Envelope {..})) = case envelopeContents of
  SomeHeartbeat f -> messageB envelopeHeader envelopeTrailer f
  SomeLogon f -> messageB envelopeHeader envelopeTrailer f

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
    MsgTypeHeartbeat -> fmap SomeHeartbeat <$> mp
    MsgTypeLogon -> fmap SomeLogon <$> mp
    _ -> fail ("Unknown message tag: " <> show typ)

class (IsMessage a) => IsAnyMessage a where
  unpackAnyMessage :: AnyMessage -> Maybe a
  packAnyMessage :: a -> AnyMessage

instance IsAnyMessage Heartbeat where
  packAnyMessage = SomeHeartbeat
  unpackAnyMessage = \case
    SomeHeartbeat f -> Just f
    _ -> Nothing

instance IsAnyMessage Logon where
  packAnyMessage = SomeLogon
  unpackAnyMessage = \case
    SomeLogon f -> Just f
    _ -> Nothing
