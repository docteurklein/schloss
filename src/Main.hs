module Main where

import Hasql.Notifications (listen, unlisten, toPgIdentifier, waitForNotifications)
-- import Network.URI (URI)
import Control.Concurrent (threadWaitRead, threadDelay)
import Control.Concurrent.Async (async)
import Control.Monad (void, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
-- import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Lens (Lens)
import Data.Aeson (FromJSON, ToJSON, encode, Value)
import Data.ByteString.Builder (toLazyByteString, string8)
import Data.Functor.Identity (Identity (..))
import Data.Generics.Product.Fields (field)
-- import Data.SirenJSON (Entity, Link, Action, Field)
import Data.Text as Text (null)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Data.Valor (Validatable, Validate, Validator, check, skip)
import Data.Vector (Vector)
-- import qualified Data.Singletons.TH as TH (genSingletons, singDecideInstances)
-- import Exinst (Dict(Dict), Dict1(dict1))
-- import HKD.Lens (makeLensesOf, LensesOf, getLensOf)
-- import Generics.OneLiner (Constraints)
import qualified Database.PostgreSQL.LibPQ as PQ
import Hasql.Connection (withLibPQConnection)
import Hasql.Generic.HasParams (HasParams, mkParams)
import Hasql.Generic.HasRow (HasRow, mkRow)
import Hasql.Pool (Pool, use, acquire, withConnection)
-- import Hasql.Queue.IO (withDequeue)
-- import Hasql.Queue.Migrate (migrationQueryString)
-- import Hasql.Queue.Session (enqueueNotify)
import Hasql.Session (sql, statement, Session(..))
import Hasql.Statement (Statement(..))
import Hasql.TH (singletonStatement, vectorStatement)
import Hasql.Transaction.Sessions (transaction, IsolationLevel(Serializable), Mode(Write))
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Simple (defaultRequest, httpNoBody, httpLBS, httpJSON, setRequestPath, setRequestHost, setRequestPort, setRequestSecure)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Media ((//))
-- import Network.HTTP.Req (req, runReq, defaultHttpConfig, POST(..), https,  (/:), ReqBodyJson(..), ignoreResponse, responseStatusCode)
import Network.Wai.EventSource (ServerEvent(..))
import Network.Wai.EventSource.EventStream (eventToBuilder)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Optics.Getter (view)
import Servant (Proxy(..), Application, throwError)
import Servant.API (MimeRender(..), MimeUnrender(..), Accept(..), Header, QueryParam, Capture, StreamGet, NewlineFraming, JSON, Get, Post, ReqBody, NoContent(..), (:>), (:<|>)(..))
import Servant.API.Stream (SourceIO)
import Servant.Client.Core (BaseUrl(..), Scheme(..))
import Servant.Client.Streaming (client, ClientM, withClientM, mkClientEnv)
import Servant.Server (serve, err500, ServerError(..), Handler, hoistServer, ServerT)
import Servant.Types.SourceT (fromAction, foreach)
import System.ReadEnvVar (readEnvDef)
import System.Log.FastLogger (ToLogStr(..), LoggerSet, defaultBufSize, newStdoutLoggerSet, pushLogStrLn)
import qualified GHC.Show
import qualified GHC.Generics as GHC (Generic)
import qualified Generics.SOP as SOP (Generic)
import qualified Hasql.Decoders as Decode (singleRow, rowVector, noResult, jsonb)
import qualified Hasql.Encoders as Encode (noParams, array, text, jsonb, element)
import qualified Hasql.Transaction as Transactional (statement)

newtype FirstName = FirstName Text
    deriving stock    (Show, GHC.Generic)
    deriving anyclass (SOP.Generic, FromJSON, ToJSON, HasParams)

newtype LastName = LastName Text
    deriving stock    (Show, GHC.Generic)
    deriving anyclass (SOP.Generic, FromJSON, ToJSON, HasParams)

newtype UserId = UserId UUID
    deriving stock    (Show, GHC.Generic)
    deriving anyclass (SOP.Generic, FromJSON, ToJSON, HasParams)

data Event
    = UserAdded UserId LastName FirstName
    | UserBanned UserId
    deriving stock    (Show, GHC.Generic)
    deriving anyclass (SOP.Generic, FromJSON, ToJSON)

data MessageInput a = MessageInput {
    name :: Text
  , topic :: Text
  , payload :: Value
} deriving stock    (Show, GHC.Generic)
  deriving anyclass (SOP.Generic, FromJSON, ToJSON, HasParams)

data Message = Message {
    message_id :: UUID
  , name :: Text
  , payload :: Value
  , topic :: Text
  , added_at :: UTCTime
} deriving stock    (Show, GHC.Generic)
  deriving anyclass (SOP.Generic, FromJSON, ToJSON, HasRow)


type Api
    = ReqBody '[JSON] (MessageInput Event) :> Post '[JSON] Message
 :<|> Get '[JSON, SirenJSON] (Vector Message)
 :<|> Capture "topic" Text
      :> QueryParam "since" Text
      :> Header "Last-Event-Id" Text
      :> StreamGet NewlineFraming EventStream (SourceIO ServerEvent)

api = Proxy @Api

data EventStream

instance Accept EventStream where
    contentType _ = "text" // "event-stream"

instance Show ServerEvent where
    show _ = "test"

instance MimeRender EventStream ServerEvent where
  mimeRender _ = maybe "" toLazyByteString . eventToBuilder

instance MimeUnrender EventStream ServerEvent where
  mimeUnrender _ bs = Right $ RetryEvent 10

data SirenJSON
instance Accept SirenJSON where
    contentType _ = "application" // "vnd.siren+json"

instance (ToJSON a) => MimeRender SirenJSON a where
    mimeRender _ = encode


data Config = Config {
    logger :: LoggerSet
  , pool :: Pool
}

main :: IO ()
main = do
    pool <- acquire (100, 1, "")
    logger <- newStdoutLoggerSet defaultBufSize
    port <- readEnvDef "PORT" 8888 :: IO Int

    manager <- newManager defaultManagerSettings
    let env = mkClientEnv manager (BaseUrl Http "localhost" port "")

    -- async . forever $ withConnection pool \conn' -> do
    --     case conn' of
    --         Left e -> error $ show e
    --         Right conn -> do
    --             listen conn $ toPgIdentifier "*"
    --             waitForNotifications publish conn

    async . forever $ withClientM (sseClient "*" Nothing Nothing) env $ \e -> case e of
        Left err -> putStrLn $ "Error: " ++ show err
        Right rs -> foreach fail (publish "*") rs

    pushLogStrLn logger $ "listening http " <> show port
    run port $ logStdoutDev . mkApp $ Config logger pool
    where
        mkApp config = serve api $ hoistServer api (`runReaderT` config) server


sseClient :: Text -> (Maybe Text) -> (Maybe Text) -> ClientM (SourceIO ServerEvent)
_ :<|> _ :<|> sseClient = client api

type AppM = ReaderT Config Handler

server :: ServerT Api AppM
server = postMessage :<|> getMessages :<|> sse
    where
        postMessage :: (MessageInput Event) -> AppM Message
        postMessage messageInput = do
            pool <- asks pool
            result <- liftIO $ use pool $ do
                message <- transaction Serializable Write $ Transactional.statement messageInput insertMessage
                -- enqueueNotify "queue_channel" Encode.jsonb [view (field @"payload") message]
                return message
            case result of
                Right message -> return message
                Left e -> do
                    logset <- asks logger
                    liftIO $ pushLogStrLn logset $ show e
                    throwError err500
            where
                insertMessage :: Statement (MessageInput Event) Message
                insertMessage = Statement sql encoder decoder True
                    where
                        sql = "insert into schloss.messages (name, topic, payload) values ($1, $2, $3) returning message_id, name, payload, topic, added_at"
                        encoder = mkParams
                        decoder = Decode.singleRow mkRow

        getMessages :: AppM (Vector Message)
        getMessages = do
            pool <- asks pool
            result <- liftIO $ use pool $ statement () selectMessages
            case result of
                Right messages -> pure messages
                Left e -> do
                    logset <- asks logger
                    liftIO $ pushLogStrLn logset $ show e
                    throwError err500
            where
                selectMessages :: Statement () (Vector Message)
                selectMessages = Statement sql encoder decoder True
                    where
                        sql = "select message_id, name, payload, topic, added_at from schloss.messages"
                        encoder = Encode.noParams
                        decoder = Decode.rowVector mkRow

        sse :: Text -> Maybe Text -> Maybe Text -> AppM (SourceIO ServerEvent)
        sse topic _since _lastEventId = do
            logset <- asks logger
            pool <- asks pool
            return $ fromAction (\_ -> False) $ do
                liftIO $ withConnection pool \conn' -> do
                    case conn' of
                        Left e -> error $ show e
                        Right conn -> do
                            listen conn $ toPgIdentifier topic
                            liftIO $ pushLogStrLn logset $ show topic
                            fetchEvents conn

        fetchEvents con = withLibPQConnection con $ forever \pqCon -> do
          mNotification <- PQ.notifies pqCon
          case mNotification of
            Nothing -> do
              mfd <- PQ.socket pqCon
              case mfd of
                Nothing  -> error "Error checking for PostgreSQL notifications"
                Just fd -> do
                  void $ threadWaitRead fd
                  void $ PQ.consumeInput pqCon
                  return CloseEvent
            Just notification -> return $ ServerEvent
                (Just $ string8  "test") -- (PQ.notifyRelname notification))
                (Just $ string8 "test") -- (PQ.notifyExtra notification)
                []


publish channel payload = do
    print (channel, payload)
    let request
            = setRequestPath "/post"
            $ setRequestSecure False
            $ setRequestHost "localhost"
            $ setRequestPort 8889
            $ defaultRequest

    httpLBS request
    pure ()
