{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FIX.App
  ( FixSettings (..),
    runFIXApp,
    FixHandle (..),
  )
where

import Conduit
import Control.Concurrent.STM.TBMQueue
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.Conduit.Network (sinkSocket, sourceSocket)
import Data.Conduit.TQueue
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import FIX.Core
import FIX.Fields.BeginSeqNo
import FIX.Fields.EndSeqNo
import FIX.Fields.HeartBtInt
import FIX.Fields.MsgSeqNum
import FIX.Fields.SendingTime
import FIX.Messages
import FIX.Messages.Envelope
import FIX.Messages.Header
import FIX.Messages.Trailer
import Network.Socket as Network (Socket, close)
import Text.Megaparsec as Megaparsec
import Text.Show.Pretty
import UnliftIO
import UnliftIO.Concurrent (threadDelay)

-- TODO handle:

-- * Wrapping and unwrapping envelopes

-- * Heartbeat

--
-- Idea: We might need to wrap the inner conduit in channels for this to
-- work nicely.

data LogLevel
  = Debug
  | Error
  deriving (Eq, Ord)

data FixSettings m = FixSettings
  { sock :: Network.Socket,
    headerPrototype :: Header,
    logMessage :: LogFunc m
  }

type LogFunc m = LogLevel -> Text -> m ()

data FixHandle m = FixHandle
  { awaitMessage :: m AnyMessage,
    sendMessage :: AnyMessage -> m ()
  }

data ProtocolException
  = ProtocolExceptionDisconnected
  | ProtocolExceptionUnparseableMessage (ParseErrorBundle ByteString Void)
  deriving (Show)

instance Exception ProtocolException where
  displayException = \case
    ProtocolExceptionDisconnected -> "Connection was broken early."
    ProtocolExceptionUnparseableMessage err ->
      unlines
        [ "Failed to parse message:",
          errorBundlePretty err
        ]

runFIXApp ::
  forall m.
  (MonadUnliftIO m) =>
  FixSettings m ->
  (FixHandle m -> m ()) ->
  m ()
runFIXApp FixSettings {..} userAppFunc = do
  -- Receive one message at a time
  receivingQueue <- liftIO $ newTBMQueueIO 1
  let receiver = do
        logMessage Debug "Receiver starting."
        runConduit $
          transPipe liftIO (sourceSocket sock)
            .| C.mapM
              ( \bs -> do
                  logMessage Debug $ T.pack $ unlines ["Received message:", ppShow bs]
                  pure bs
              )
            .| anyMessageSource
            .| C.mapM
              ( \case
                  Left err -> throwIO $ ProtocolExceptionUnparseableMessage err
                  Right am -> do
                    logMessage Debug $ T.pack $ unlines ["Decoded message:", ppShow am]
                    pure am
              )
            .| sinkTBMQueue receivingQueue
        logMessage Debug "Receiver done."
      awaitOutsideMessage :: STM (Maybe (Envelope AnyMessage))
      awaitOutsideMessage = readTBMQueue receivingQueue

  -- Send one message at a time
  sendingQueue <- liftIO $ newTBMQueueIO 1
  let sender = do
        logMessage Debug "Sender starting."
        runConduit $
          transPipe liftIO (sourceTBMQueue sendingQueue)
            .| C.mapM
              ( \am -> do
                  logMessage Debug $ T.pack $ unlines ["Encoding message:", ppShow am]
                  pure am
              )
            .| transPipe liftIO anyMessageSink
            .| C.mapM
              ( \bs -> do
                  logMessage Debug $ T.pack $ unlines ["Sending message:", ppShow bs]
                  pure bs
              )
            .| transPipe liftIO (sinkSocket sock)
        logMessage Debug "Sender done."
      sendMessageOut :: Envelope AnyMessage -> m ()
      sendMessageOut eam = do
        logMessage Debug "Enqueueing message to send out."
        atomically $ writeTBMQueue sendingQueue eam
        logMessage Debug "Enqueued message to send out."

  -- We use race_ because the receiver will not stop if the socket stays open.
  -- The sender can also stop early in case the socket is closed early, but in
  -- that case we want the receiver to stop anyway.
  let interacter = race_ receiver sender

  appInputQueue <- newTBQueueIO 1
  let passMessageToApp :: AnyMessage -> m ()
      passMessageToApp = atomically . writeTBQueue appInputQueue
      awaitMessage :: m AnyMessage
      awaitMessage = atomically $ readTBQueue appInputQueue
  appOutputQueue <- liftIO $ newTBMQueueIO 1
  let sendMessage :: AnyMessage -> m ()
      sendMessage = atomically . writeTBMQueue appOutputQueue
      readMessageFromApp :: STM (Maybe AnyMessage)
      readMessageFromApp = readTBMQueue appOutputQueue
  let fixHandle = FixHandle {..}
  let userAppThread :: m ()
      userAppThread = do
        logMessage Debug "User app starting."
        userAppFunc fixHandle
        atomically $ closeTBMQueue appOutputQueue
        logMessage Debug "User app done."

  let systemHandlerThread :: m ()
      systemHandlerThread = do
        logMessage Debug "System handler starting."
        systemHandler
          sendMessage
          awaitOutsideMessage
          passMessageToApp
          readMessageFromApp
          sendMessageOut
        atomically $ closeTBMQueue receivingQueue
        atomically $ closeTBMQueue sendingQueue
        logMessage Debug "System handler done."

  -- Concurently because the interacter stops (eventually) when the system
  -- handler is done.
  let systemThreads :: m ()
      systemThreads = concurrently_ interacter systemHandlerThread

  concurrently_ systemThreads userAppThread
    `finally` liftIO (Network.close sock)
  where
    systemHandler ::
      (AnyMessage -> m ()) ->
      STM (Maybe (Envelope AnyMessage)) ->
      (AnyMessage -> m ()) ->
      STM (Maybe AnyMessage) ->
      (Envelope AnyMessage -> m ()) ->
      m ()
    systemHandler sendSystemMessage awaitOutsideMessage passMessageToApp readMessageFromApp sendMessageOut = go (MsgSeqNum 1) M.empty
      where
        -- Pass in the "next" message sequence number to use when sending a
        -- message or to expect when receiving a message.
        go :: MsgSeqNum -> Map Word (Envelope AnyMessage) -> m ()
        go nextInSeqNum outMessages = do
          inOrOut <-
            atomically $
              (Left <$> awaitOutsideMessage)
                `orElse` (Right <$> readMessageFromApp)
          case inOrOut of
            Right Nothing -> do
              logMessage Debug "App finished early, without receiving a logout message."
            Right (Just msgOut) -> do
              logMessage Debug "Got a message from the user app to send out."
              let msgType = anyMessageType msgOut
              let nextOutSeqNum = succ $ maybe 0 fst $ M.lookupMax outMessages
              now <- liftIO getCurrentTime
              let header =
                    headerPrototype
                      { headerMsgSeqNum = MsgSeqNum nextOutSeqNum,
                        headerMsgType = msgType,
                        headerSendingTime = SendingTime (UTCTimestamp now)
                      }
              let envelopeOut =
                    Envelope
                      { envelopeHeader = header,
                        envelopeContents = msgOut,
                        envelopeTrailer =
                          -- Empty trailer
                          -- checksum will be fixed my by messageB
                          makeTrailer $ renderCheckSum 0
                      }
              let newOutMessages = M.insert nextOutSeqNum envelopeOut outMessages
              sendMessageOut envelopeOut
              go nextInSeqNum newOutMessages
            Left mMsgIn -> do
              case mMsgIn of
                Nothing -> throwIO ProtocolExceptionDisconnected
                Just msgIn -> do
                  logMessage Debug "Got a message from the connection to send to the app."
                  -- TODO check msgSeqNum here
                  let contents = envelopeContents msgIn
                  let pass = passMessageToApp contents
                  let continue = go (incrementMsgSeqNum nextInSeqNum) outMessages
                  -- TODO handle system messages here
                  case contents of
                    SomeLogon logon ->
                      withHeartbeatThread sendSystemMessage (logonHeartBtInt logon) $ do
                        pass
                        continue
                    SomeTestRequest testRequest -> do
                      sendSystemMessage $ packAnyMessage $ makeHeartbeat {heartbeatTestReqID = Just (testRequestTestReqID testRequest)}
                      -- No need to pass
                      continue
                    SomeResendRequest resendRequest -> do
                      let begin = unBeginSeqNo $ resendRequestBeginSeqNo resendRequest
                      let end = unEndSeqNo $ resendRequestEndSeqNo resendRequest
                      logMessage Debug $
                        T.pack $
                          unlines
                            [ "Got a resend request for",
                              if end == 0
                                then unwords ["all messages from", show begin, "onwards"]
                                else
                                  if begin == end
                                    then unwords ["message", show begin]
                                    else unwords ["messages from", show begin, "to", show end],
                              "this is not implemented yet."
                            ]
                      let selectedMessages
                            | end == 0 = M.elems $ snd $ M.split (begin - 1) outMessages
                            | begin == end = maybe [] pure $ M.lookup begin outMessages
                            | otherwise =
                                M.elems $
                                  fst $
                                    M.split (end + 1) $
                                      snd $
                                        M.split (begin - 1) outMessages
                      -- We need to use 'sendMessageOut' instead of
                      -- 'sendSystemMessage' because the original header (with
                      -- sequence number and sending time) must be preserved.
                      mapM_ sendMessageOut selectedMessages
                      -- No need to pass
                      continue
                    SomeLogout _ -> do
                      pass
                      pure () -- Done, don't continue
                    _ -> do
                      pass
                      continue

withHeartbeatThread :: (MonadUnliftIO m) => (AnyMessage -> m ()) -> HeartBtInt -> m () -> m ()
withHeartbeatThread sendSystemMessage (HeartBtInt seconds) func =
  let heartBeatThread = forever $ do
        threadDelay (seconds * 1_000_000)
        sendSystemMessage $ packAnyMessage makeHeartbeat
   in race_ heartBeatThread func

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
                    -- there is not enough input because we may still get more
                    -- data from the socket.
                    TrivialError _ (Just EndOfInput) _ -> True
                    _ -> False
              if all isEOFError (bundleErrors err)
                then pure ()
                else yield res
          go afterState

anyMessageSink :: (PrimMonad m) => ConduitT (Envelope AnyMessage) ByteString m ()
anyMessageSink =
  C.map anyMessageB
    .| C.map BB.toLazyByteString
    .| C.map LB.toStrict
