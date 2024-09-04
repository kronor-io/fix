{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FIX.Conduit where

import Conduit
import Data.ByteString (ByteString)
import qualified Data.Conduit.Combinators as C
import FIX.Messages
import FIX.Messages.Envelope
import Text.Megaparsec as Megaparsec

anyMessageSource ::
  forall m.
  (Monad m) =>
  ConduitT ByteString (Either (ParseErrorBundle ByteString Void) (Envelope AnyMessage)) m ()
anyMessageSource = go initialState
  where
    initialState :: State ByteString e
    initialState =
      State
        { stateInput = "",
          stateOffset = 0,
          statePosState =
            PosState
              { pstateInput = "",
                pstateOffset = 0,
                pstateSourcePos = initialPos "",
                pstateTabWidth = defaultTabWidth,
                pstateLinePrefix = ""
              },
          stateParseErrors = []
        }
    go :: State ByteString Void -> ConduitT ByteString (Either (ParseErrorBundle ByteString Void) (Envelope AnyMessage)) m ()

    go beforeState = do
      mBs <- await
      case mBs of
        Nothing -> pure ()
        Just sb -> do
          let stateWithInput = beforeState {stateInput = stateInput beforeState <> sb}
          let (afterState, res) = runParser' anyMessageP stateWithInput
          case res of
            Right _ -> yield res
            Left err -> do
              let isEOFError = \case
                    -- No need to report errors that only happened because
                    -- there is not enough input.
                    TrivialError _ (Just EndOfInput) _ -> False
                    _ -> True
              if all isEOFError (bundleErrors err)
                then pure ()
                else yield res
          go afterState

anyMessageSink :: (PrimMonad m) => ConduitT (Envelope AnyMessage) ByteString m ()
anyMessageSink = C.map anyMessageB .| builderToByteString
