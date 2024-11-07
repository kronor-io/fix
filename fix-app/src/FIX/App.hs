{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FIX.App (
    FixSettings (..),
    runFIXApp,
    FixHandle (..),
    LogLevel (..),
)
where

import Conduit
import Control.Concurrent.STM.TBMQueue
import Control.Monad
import qualified Control.Monad.Combinators as PC
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.Conduit.Network (sinkSocket, sourceSocket)
import Data.Conduit.TQueue
import Data.Map (Map)
import qualified Data.Map.Strict as M
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
import qualified Network.TLS as TLS
import Text.Megaparsec as Megaparsec
import Text.Show.Pretty
import UnliftIO
import UnliftIO.Concurrent (threadDelay)


data LogLevel
    = Debug
    | VeryVerbose
    | Error
    deriving (Eq, Ord)


data InOrOut
    = In (Envelope AnyMessage)
    | Out AnyMessage


data FixSettings m = FixSettings
    { sock :: Network.Socket
    , headerPrototype :: Header
    , logMessage :: LogFunc m
    , tlsContext :: !(Maybe TLS.Context)
    }


type LogFunc m = LogLevel -> Text -> m ()


data FixHandle m = FixHandle
    { awaitMessage :: m AnyMessage
    , sendMessage :: AnyMessage -> m ()
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
                [ "Failed to parse message:"
                , errorBundlePretty err
                ]


receiverSource :: MonadUnliftIO m => Network.Socket -> Maybe TLS.Context -> ConduitT void ByteString m ()
receiverSource sock mContext = case mContext of
    Nothing -> transPipe liftIO (sourceSocket sock)
    Just ctx ->
        let loop = do
                bs <- lift $ TLS.recvData ctx
                if SB.null bs
                    then return ()
                    else yield bs >> loop
         in loop


senderSink :: MonadUnliftIO m => Network.Socket -> Maybe TLS.Context -> ConduitT ByteString void m ()
senderSink sock mContext = case mContext of
    Nothing -> transPipe liftIO (sinkSocket sock)
    Just ctx ->
        let loop = await >>= maybe (return ()) (\bs -> lift (TLS.sendData ctx (LB.fromStrict bs)) >> loop)
         in loop


runFIXApp ::
    forall m a.
    MonadUnliftIO m =>
    FixSettings m ->
    (FixHandle m -> m a) ->
    m a
runFIXApp FixSettings{..} userAppFunc = do
    messagesQueue <- liftIO $ newTBMQueueIO 1000
    let receiver = do
            logMessage Debug "Receiver starting."
            runConduit $
                receiverSource sock tlsContext
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
                                logMessage VeryVerbose $ T.pack $ unlines ["Decoded message:", ppShow am]
                                pure am
                        )
                    .| concatC
                    .| C.map In
                    .| sinkTBMQueue messagesQueue
            logMessage Debug "Receiver done."

    -- Send one message at a time
    sendingQueue <- liftIO $ newTBMQueueIO 1000

    let sender = do
            logMessage Debug "Sender starting."
            runConduit $
                transPipe liftIO (sourceTBMQueue sendingQueue)
                    .| C.mapM
                        ( \am -> do
                            logMessage VeryVerbose $ T.pack $ unlines ["Encoding message:", ppShow am]
                            pure am
                        )
                    .| transPipe liftIO anyMessageSink
                    .| C.mapM
                        ( \bs -> do
                            logMessage Debug $ T.pack $ unlines ["Sending message:", ppShow bs]
                            pure bs
                        )
                    .| senderSink sock tlsContext
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

    appInputQueue <- newTBQueueIO 1000
    let passMessageToApp :: AnyMessage -> m ()
        passMessageToApp x = atomically $ writeTBQueue appInputQueue x

        awaitMessage :: m AnyMessage
        awaitMessage = atomically $ readTBQueue appInputQueue

    let sendMessage :: AnyMessage -> m ()
        sendMessage = atomically . writeTBMQueue messagesQueue . Out

        readInOrOutMessage :: STM (Maybe InOrOut)
        readInOrOutMessage = readTBMQueue messagesQueue

    let fixHandle = FixHandle{..}

    let userAppThread :: m a
        userAppThread = do
            res <- userAppFunc fixHandle
            atomically $ closeTBMQueue messagesQueue
            return res

    let systemHandlerThread :: m ()
        systemHandlerThread = do
            logMessage Debug "System handler starting."

            systemHandler
                sendMessage
                readInOrOutMessage
                passMessageToApp
                sendMessageOut

            atomically $ do
                closeTBMQueue messagesQueue
                closeTBMQueue sendingQueue
            logMessage Debug "System handler done."

    -- Concurently because the interacter stops (eventually) when the system
    -- handler is done.
    let systemThreads :: m ()
        systemThreads = concurrently_ interacter systemHandlerThread

    res <- race systemThreads userAppThread
    case res of
        Left () -> throwIO ProtocolExceptionDisconnected
        Right a -> return a
  where
    systemHandler ::
        (AnyMessage -> m ()) ->
        STM (Maybe InOrOut) ->
        (AnyMessage -> m ()) ->
        (Envelope AnyMessage -> m ()) ->
        m ()
    systemHandler sendSystemMessage readInOrOutMessage passMessageToApp sendMessageOut = go (MsgSeqNum 1) M.empty
      where
        -- Pass in the "next" message sequence number to use when sending a
        -- message or to expect when receiving a message.
        go :: MsgSeqNum -> Map Word (Envelope AnyMessage) -> m ()
        go !nextInSeqNum !outMessages = do
            inOrOut <- atomically readInOrOutMessage

            case inOrOut of
                Nothing -> do
                    logMessage Debug "App finished early, without receiving a logout message."
                    throwIO ProtocolExceptionDisconnected
                (Just (Out msgOut)) -> do
                    logMessage Debug "Got a message from the user app to send out."
                    let msgType = anyMessageType msgOut
                    let !nextOutSeqNum = succ $ maybe 0 fst $ M.lookupMax outMessages
                    now <- liftIO getCurrentTime
                    let header =
                            headerPrototype
                                { headerMsgSeqNum = MsgSeqNum nextOutSeqNum
                                , headerMsgType = msgType
                                , headerSendingTime = SendingTime (UTCTimestamp now)
                                }
                    let envelopeOut =
                            Envelope
                                { envelopeHeader = header
                                , envelopeContents = msgOut
                                , envelopeTrailer =
                                    -- Empty trailer
                                    -- checksum will be fixed my by messageB
                                    makeTrailer $ renderCheckSum 0
                                }
                    let newOutMessages = M.insert nextOutSeqNum envelopeOut outMessages
                    sendMessageOut envelopeOut
                    go nextInSeqNum newOutMessages
                (Just (In msgIn)) -> do
                    if headerMsgSeqNum (envelopeHeader msgIn) == nextInSeqNum
                        then do
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
                                    sendSystemMessage $ packAnyMessage $ makeHeartbeat{heartbeatTestReqID = Just (testRequestTestReqID testRequest)}
                                    -- No need to pass
                                    continue
                                SomeResendRequest resendRequest -> do
                                    let begin = unBeginSeqNo $ resendRequestBeginSeqNo resendRequest
                                    let end = unEndSeqNo $ resendRequestEndSeqNo resendRequest
                                    logMessage Debug $
                                        T.pack $
                                            unlines
                                                [ "Got a resend request for"
                                                , if end == 0
                                                    then unwords ["all messages from", show begin, "onwards"]
                                                    else
                                                        if begin == end
                                                            then unwords ["message", show begin]
                                                            else unwords ["messages from", show begin, "to", show end]
                                                , "this is not implemented yet."
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
                        else do
                            -- Here we ignore messages that have the wrong (not the
                            -- next) sequence number and ask the other party to
                            -- resend it and all future messages.
                            -- A more sophisticated approach would involve only
                            -- asking for messages we missed, but this approach is
                            -- "good enough" for now.
                            sendSystemMessage $
                                packAnyMessage $
                                    makeResendRequest
                                        (BeginSeqNo (unMsgSeqNum nextInSeqNum))
                                        -- 0 as the end means "all others after the begin"
                                        (EndSeqNo 0)
                            go nextInSeqNum outMessages


withHeartbeatThread :: MonadUnliftIO m => (AnyMessage -> m ()) -> HeartBtInt -> m () -> m ()
withHeartbeatThread sendSystemMessage (HeartBtInt seconds) func =
    let heartBeatThread = forever $ do
            threadDelay (seconds * 1_000_000)
            sendSystemMessage $ packAnyMessage makeHeartbeat
     in race_ heartBeatThread func


anyMessageSource ::
    forall m.
    Monad m =>
    ConduitT ByteString (Either (ParseErrorBundle ByteString Void) [Envelope AnyMessage]) m ()
anyMessageSource = go initialState
  where
    initialState :: State ByteString e
    initialState =
        State
            { stateInput = ""
            , stateOffset = 0
            , statePosState =
                PosState
                    { pstateInput = ""
                    , pstateOffset = 0
                    , pstateSourcePos = initialPos ""
                    , pstateTabWidth = defaultTabWidth
                    , pstateLinePrefix = ""
                    }
            , stateParseErrors = []
            }
    go :: State ByteString Void -> ConduitT ByteString (Either (ParseErrorBundle ByteString Void) [Envelope AnyMessage]) m ()

    go beforeState = do
        mBs <- await
        case mBs of
            Nothing -> pure ()
            Just sb -> do
                let stateWithInput = beforeState{stateInput = stateInput beforeState <> sb}
                let (afterState, res) = runParser' (PC.many anyMessageP) stateWithInput
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


anyMessageSink :: PrimMonad m => ConduitT (Envelope AnyMessage) ByteString m ()
anyMessageSink =
    C.map anyMessageB
        .| C.map BB.toLazyByteString
        .| C.map LB.toStrict
