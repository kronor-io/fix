{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FIX.Conduit where

import Conduit
import Control.Concurrent.Chan
import Control.Concurrent.STM.TBMQueue
import Data.ByteString (ByteString)
import qualified Data.Conduit.Combinators as C
import Data.Conduit.Network
import Data.Conduit.TQueue
import FIX.Fields.CheckSum
import FIX.Fields.MsgSeqNum
import FIX.Fields.MsgType
import FIX.Messages
import FIX.Messages.Envelope
import FIX.Messages.Header
import FIX.Messages.Trailer
import Network.Socket as Network
import System.Exit
import Text.Megaparsec as Megaparsec
import UnliftIO

-- TODO handle:

-- * Wrapping and unwrapping envelopes

-- * Heartbeat

--
-- Idea: We might need to wrap the inner conduit in channels for this to
-- work nicely.

data FixHandle m = FixHandle
  { awaitMessage :: m AnyMessage,
    sendMessage :: AnyMessage -> m ()
  }

runFIXApp ::
  forall m.
  (MonadUnliftIO m) =>
  Network.Socket ->
  Header ->
  (FixHandle m -> m ()) ->
  m ()
runFIXApp sock headerPrototype userAppFunc = do
  -- Receive one message at a time
  receivingQueue <- liftIO $ newTBMQueueIO 1
  let receiver =
        liftIO $
          runConduit $
            sourceSocket sock
              .| anyMessageSource
              .| C.mapM
                ( \case
                    Left err ->
                      die $
                        unlines
                          [ "Failed to parse message: ",
                            errorBundlePretty err
                          ]
                    Right am -> pure am
                )
              .| sinkTBMQueue receivingQueue
      awaitOutsideMessage :: STM (Maybe (Envelope AnyMessage))
      awaitOutsideMessage = readTBMQueue receivingQueue

  -- Send one message at a time
  sendingQueue <- liftIO $ newTBMQueueIO 1
  let sender =
        liftIO $
          runConduit $
            sourceTBMQueue sendingQueue
              .| anyMessageSink
              .| sinkSocket sock
      sendMessageOut :: Envelope AnyMessage -> m ()
      sendMessageOut = atomically . writeTBMQueue sendingQueue

  -- Concurrently because the receiver stops when the socket is closed and the
  -- sender stops when the queue is closed by the system handler.
  let interacter = concurrently_ receiver sender

  appInputQueue <- newTBQueueIO 1
  let passMessageToApp :: AnyMessage -> m ()
      passMessageToApp = atomically . writeTBQueue appInputQueue
      awaitMessage :: m AnyMessage
      awaitMessage = atomically $ readTBQueue appInputQueue
  appOutputQueue <- newTBQueueIO 1
  let sendMessage :: AnyMessage -> m ()
      sendMessage = atomically . writeTBQueue appOutputQueue
      readMessageFromApp :: STM AnyMessage
      readMessageFromApp = readTBQueue appOutputQueue
  let fixHandle = FixHandle {..}
  let userAppThread :: m ()
      userAppThread = userAppFunc fixHandle

  let systemHandlerThread :: m ()
      systemHandlerThread =
        systemHandler
          awaitOutsideMessage
          passMessageToApp
          readMessageFromApp
          sendMessageOut

  -- Concurently because the interacter stops (eventually) when the system
  -- handler is done.
  let systemThreads :: m ()
      systemThreads = concurrently_ interacter systemHandlerThread

  concurrently_ systemThreads userAppThread
  where
    systemHandler awaitOutsideMessage passMessageToApp readMessageFromApp sendMessageOut = go (MsgSeqNum 0)
      where
        -- Pass in the "next" message sequence number to use when sending a
        -- message or to expect when receiving a message.
        go :: MsgSeqNum -> m ()
        go nextSeqNum = do
          inOrOut <-
            atomically $
              (Left <$> awaitOutsideMessage)
                `orElse` (Right <$> readMessageFromApp)
          case inOrOut of
            Right msgOut -> do
              let msgType = anyMessageType msgOut
              let header =
                    headerPrototype
                      { headerMsgSeqNum = nextSeqNum,
                        headerMsgType = msgType
                      }
              let envelopeOut =
                    Envelope
                      { envelopeHeader = header,
                        envelopeContents = msgOut,
                        envelopeTrailer =
                          -- Empty trailer
                          -- checksum will be fixed my by messageB
                          Trailer
                            { trailerSignatureLength = Nothing,
                              trailerSignature = Nothing,
                              trailerCheckSum = renderCheckSum 0
                            }
                      }
              sendMessageOut envelopeOut
              go (incrementMsgSeqNum nextSeqNum)
            Left mMsgIn -> do
              case mMsgIn of
                Nothing -> error "Connection was broken early."
                Just msgIn -> do
                  -- TODO check msgSeqNum here
                  let contents = envelopeContents msgIn
                  -- TODO handle system messages here
                  case contents of
                    SomeLogout _ -> pure () -- Done
                    _ -> do
                      passMessageToApp contents
                      go (incrementMsgSeqNum nextSeqNum)

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
