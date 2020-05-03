module Main where

import Data.Vector (Vector)
import Data.ByteString.Builder (toLazyByteString)
import Data.UUID (UUID)
import Data.Text as Text (null)
-- import Data.Profunctor (dimap, rmap)
import Data.Time.Clock (UTCTime)
-- import Data.UUID.V4 (nextRandom)
import Control.Concurrent.Async (async)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans.Except (ExceptT, throwE)
-- import Control.Monad.Trans.Resource (ResourceT, runResourceT, allocate, release)
import Control.Monad (void, forever)
import Control.Concurrent (threadWaitRead, threadDelay)
import Optics.Getter (view)
import Data.Generics.Product.Fields (field)

import Network.Wai.Handler.Warp (runEnv)
import Network.Wai.EventSource (ServerEvent(..))
import Network.Wai.EventSource.EventStream (eventToBuilder)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Hasql.Session (sql, statement, run, Session(..))
import qualified Hasql.Transaction as Transactional (statement)
import Hasql.Transaction.Sessions (transaction, IsolationLevel(Serializable), Mode(Write))
import Hasql.Statement (Statement(..))
import Hasql.Queue.Session (enqueueNotify)
import Hasql.Queue.IO (withDequeue)
import Hasql.Queue.Migrate (migrationQueryString)
import qualified Hasql.Encoders as Encode (noParams, array, text, jsonb, element)
import qualified Hasql.Decoders as Decode (singleRow, rowVector, noResult, jsonb)
import Hasql.TH (singletonStatement, vectorStatement)
import Hasql.Generic.HasRow (HasRow, mkRow)
import Hasql.Generic.HasParams (HasParams, mkParams)
-- import Hasql.Connection (acquire, release, Connection)
-- import qualified Database.PostgreSQL.LibPQ as PQ
-- import Hasql.Notification (notificationChannel, notificationData, getNotification)
-- import Hasql.Notifications (listen, unlisten, toPgIdentifier)
import Hasql.Pool (Pool, use, acquire, withConnection)
-- import Data.Pool (withResource)
import Data.SirenJSON (Entity, Link, Action, Field)
-- import Network.URI (URI)
import Data.Aeson (FromJSON, ToJSON, encode, Value)
import Data.Valor (Validatable, Validate, Validator, check, skip)
import Data.Functor.Identity (Identity (..))
import qualified GHC.Generics as GHC (Generic)
import qualified Generics.SOP as SOP (Generic)
import Servant (MimeRender(..), Accept(..), ServerT, Handler, Application
      , Proxy(..), Header, QueryParam, Capture, StreamGet, NewlineFraming
      , JSON, Get, Post, ReqBody, NoContent(..), ServerError(..), err500
      , throwError, serve, hoistServer, (:>), (:<|>)(..))
import Servant.API.Stream (SourceIO)
import Network.HTTP.Media ((//))
import Servant.Types.SourceT (fromAction)
import System.Log.FastLogger (ToLogStr(..), LoggerSet, defaultBufSize, newStdoutLoggerSet, pushLogStrLn)
import Network.HTTP.Req (req, runReq, defaultHttpConfig, POST(..), https,  (/:), ReqBodyJson(..), ignoreResponse, responseStatusCode)


type Api = ReqBody '[JSON] MessageInput :> Post '[JSON] Message
      :<|> Get '[JSON, SirenJSON] (Vector Message)
      :<|> Capture "topic" Text
           :> QueryParam "since" Text
           :> Header "Last-Event-Id" Text
           :> StreamGet NewlineFraming EventStream (SourceIO ServerEvent)

data EventStream

instance Accept EventStream where
    contentType _ = "text" // "event-stream"

instance MimeRender EventStream ServerEvent where
  mimeRender _ = maybe "" toLazyByteString . eventToBuilder

data SirenJSON
instance Accept SirenJSON where
    contentType _ = "application" // "vnd.siren+json"

instance (ToJSON a) => MimeRender SirenJSON a where
    mimeRender _ = encode

-- data MessageInput' a = MessageInput {
--     message_id :: Validatable a Text (Maybe UUID)
--   , payload :: Validatable a Text Value
--   , topic :: Validatable a Text Text
-- } -- deriving stock    (Show, GHC.Generic)
--   -- deriving anyclass (SOP.Generic, FromJSON, ToJSON, HasParams)
-- 
-- type MessageInput = MessageInput' Identity
-- deriving instance Show MessageInput
-- deriving via MessageInput instance FromJSON MessageInput
-- deriving via MessageInput instance ToJSON MessageInput
-- deriving via MessageInput instance HasParams MessageInput
-- 
-- nonempty' :: Monad m => Text -> ExceptT Text m Text
-- nonempty' t = if Text.null t
--   then throwE "can't be empty"
--   else pure t
-- 
-- messageInputValidator :: Monad m => Validator MessageInput m MessageInputError
-- messageInputValidator = MessageInput
--   <$> skip
--   <*> skip
--   <*> check topic nonempty'
-- 
-- type MessageInputError = MessageInput' Validate
-- deriving instance Show MessageInputError
-- deriving via MessageInputError instance ToJSON MessageInputError

data MessageInput = MessageInput {
    name :: Text
  , payload :: Value
  , topic :: Text
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

data Config = Config {
    logger :: LoggerSet
  , pool :: Pool
}

main :: IO ()
main = do
    pool <- acquire (100, 1, "")
    logger <- newStdoutLoggerSet defaultBufSize

    use pool $ do
        sql $ fromString $ migrationQueryString "jsonb"

    async . forever $ do
        withConnection pool \conn -> do
            case conn of
                Right conn' -> withDequeue "queue_channel" conn' Decode.jsonb 10 10 publish
                Left e -> error $ show e

    pushLogStrLn logger $ "listening on port 8888"
    runEnv 8888 $ logStdoutDev . mkApp $ Config logger pool
    where
        api = Proxy @Api
        mkApp config = serve api $ hoistServer api (`runReaderT` config) server


type AppM = ReaderT Config Handler

server :: ServerT Api AppM
server = postMessage :<|> getMessages :<|> sse
    where
        postMessage :: MessageInput -> AppM Message
        postMessage messageInput = do
            pool <- asks pool
            result <- liftIO $ use pool $ do
                message <- transaction Serializable Write $ Transactional.statement messageInput insertMessage
                enqueueNotify "queue_channel" Encode.jsonb [view (field @"payload") message]
                return message
            case result of
                Right message -> return message
                Left e -> do
                    logset <- asks logger
                    liftIO $ pushLogStrLn logset $ show e
                    throwError err500
            where
                insertMessage :: Statement MessageInput Message
                insertMessage = Statement sql encoder decoder True
                    where
                        sql = "insert into schloss.messages (name, payload, topic) values ($1, $2, $3) returning message_id, name, payload, topic, added_at"
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
            return $ fromAction (\_ -> False) $ do
                threadDelay 1_000_000
                -- liftIO $ pushLogStrLn logset $ "sse"
                return CloseEvent

publish payload = runReq defaultHttpConfig $ do
    r <- req POST
        (https "httpbin.org" /: "post")
        (ReqBodyJson payload)
        ignoreResponse
        mempty
    liftIO $ print (responseStatusCode r)
