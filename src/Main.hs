module Main where

import PostgreSQL.Binary.Data (Vector)
import Data.ByteString.Builder (toLazyByteString)
import Data.UUID (UUID, fromString)
import Data.Time.Clock (UTCTime)
-- import Data.UUID.V4 (nextRandom)
-- import Control.Concurrent.Async (async)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT, allocate, release)
import Control.Monad (void, forever)
import Control.Concurrent (threadWaitRead, threadDelay)
-- import Optics.Getter (view)
-- import Data.Generics.Product.Fields (field)

import Network.Wai.Handler.Warp (runEnv)
import Network.Wai.EventSource (ServerEvent(..))
import Network.Wai.EventSource.EventStream (eventToBuilder)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Hasql.Session (sql, statement, run, Session(..))
import Hasql.Statement (Statement(..))
import Hasql.Encoders (noParams)
import Hasql.Decoders (singleRow, rowVector)
import Hasql.TH (uncheckedSql, singletonStatement, vectorStatement)
import Hasql.Generic.HasRow (HasRow, mkRow)
import Hasql.Generic.HasParams (HasParams, mkParams)
-- import qualified Hasql.Connection as Connection (acquire, withLibPQConnection)
-- import qualified Database.PostgreSQL.LibPQ as PQ
-- import Hasql.Notification (notificationChannel, notificationData, getNotification)
import Hasql.Notifications (listen, unlisten, toPgIdentifier)
import Hasql.Pool (Pool, use, acquire)
import Data.SirenJSON (Entity, Link, Action, Field)
-- import Network.URI (URI)
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.Valor (Validatable, Validate)
import Data.Functor.Identity (Identity (..))
import qualified GHC.Generics as GHC (Generic)
import qualified Generics.SOP as SOP (Generic)
import Servant (MimeRender(..), Accept(..), ServerT, Handler, Application
      , Proxy(..), Header, QueryParam, Capture, StreamGet, NewlineFraming
      , JSON, Get, Post, ReqBody, NoContent(..), ServerError(..), err500
      , throwError, serve, hoistServer, (:>), (:<|>)(..)
      )
import Servant.API.Stream (SourceIO, toSourceIO)
import Network.HTTP.Media ((//))
import Servant.Types.SourceT (fromAction)
import System.Log.FastLogger (ToLogStr(..), LoggerSet, defaultBufSize, newStdoutLoggerSet, pushLogStrLn)


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
--     message_id :: Validatable a String (Maybe UUID)
--   , content :: Validatable a [String] Text
--   , topic :: Validatable a [String] Text
-- }
-- 
-- type MessageInput = MessageInput' Identity
-- deriving instance Show MessageInput
-- deriving instance GHC.Generic MessageInput
-- deriving instance SOP.Generic MessageInput
-- deriving instance FromJSON MessageInput
-- deriving instance ToJSON MessageInput
-- deriving instance HasParams MessageInput

-- type MessageInputError = MessageInput' Validate
-- deriving instance Show MessageInputError


data MessageInput = MessageInput {
    content :: Text
  , topic :: Text
} deriving (Show, GHC.Generic, SOP.Generic, FromJSON, ToJSON, HasParams)

data Message = Message {
    message_id :: UUID
  , content :: Text
  , topic :: Text
  , at :: UTCTime
} deriving (Show, GHC.Generic, SOP.Generic, FromJSON, ToJSON, HasRow)

data Config = Config {
    logger :: LoggerSet
  , pool :: Pool
}

initSchema = [uncheckedSql|
    create table if not exists message (
        message_id uuid primary key default uuid_generate_v4(),
        content text not null,
        topic text not null,
        at timestamptz not null default clock_timestamp()
    );
    create or replace rule immutable_message as on update to message do instead nothing;
    create or replace rule immortal_message as on delete to message do instead nothing;

    create or replace function notify_message() returns trigger language plpgsql as $$
    begin
        perform pg_notify(new.topic, new.message_id::text);
        return null;
    end;
    $$;

    drop trigger if exists on_message_insert on message;
    create trigger on_message_insert after insert on message
    for each row execute function notify_message();
|]

main :: IO ()
main = do
    pool <- acquire (1, 1, "")
    _ <- use pool $ sql initSchema
    logger <- newStdoutLoggerSet defaultBufSize
    runEnv 8888 $ logStdoutDev . mkApp $ Config logger pool
    where
        api = Proxy @Api
        mkApp config = serve api $ hoistServer api (flip runReaderT config) server

type AppM = ReaderT Config Handler

server :: ServerT Api AppM
server = postMessage :<|> getMessages :<|> sse
    where
        postMessage :: MessageInput -> AppM Message
        postMessage input = do
            pool <- asks pool
            result <- liftIO $ use pool $ statement input insertMessage
              --   view (field @"content") input
              -- , view (field @"topic") input)
            case result of
                Right message -> return message
                Left e -> do
                    logset <- asks logger
                    liftIO $ pushLogStrLn logset $ show e
                    throwError err500
            where
                -- insertMessage :: Statement (Text, Text) (Text, Text)
                -- insertMessage = [singletonStatement|
                --     insert into message
                --     (content  , topic) values
                --     ($1::text , $2::text)
                --     returning content :: text, topic :: text
                -- |]
                insertMessage = Statement sql encoder decoder True
                    where
                        sql = "insert into message (content, topic) values ($1, $2) returning *"
                        encoder = mkParams
                        decoder = singleRow mkRow

        getMessages :: AppM (Vector Message)
        getMessages = do
            pool <- asks pool
            result <- liftIO $ use pool $ statement () selectMessages
            case result of
                Right messages -> pure messages
                Left e -> throwError err500 {errBody = show e}
            where
                selectMessages :: Statement () (Vector (Message))
                -- selectMessages = [vectorStatement|select content::text from message|]
                selectMessages = Statement sql encoder decoder True
                    where
                        sql = "select * from message"
                        encoder = noParams
                        decoder = rowVector mkRow

        sse :: Text -> Maybe Text -> Maybe Text -> AppM (SourceIO ServerEvent)
        sse topic _since _lastEventId = do
            -- pool <- asks pool
            -- liftIO $ use pool $ sql $ "listen " <> (fromPgIdentifier . toPgIdentifier $ topic)

            -- runResourceT $ do
            --     (releaseKey, resource) <- allocate (do
            --             conn <- Connection.acquire ""
            --             case conn of
            --                 Right conn' -> do
            --                     listen conn' $ toPgIdentifier topic
            --                     return conn')
            --         (\conn -> unlisten conn $ toPgIdentifier topic)
            --     release releaseKey

            -- logset <- asks logger
            return $ fromAction (\_ -> False) $ do
                threadDelay 1_000_000
                -- liftIO $ pushLogStrLn logset $ "sse"
                return CloseEvent


            -- conn <- liftIO $ Connection.acquire ""
            -- case conn of
            --     Right conn' -> do
            --         liftIO $ listen conn' $ toPgIdentifier topic
            --         return $ fromAction (\_ -> False) $ do
            --             waitForNotifications conn'
            --             unlisten conn' $ toPgIdentifier topic
            --             return CloseEvent
            --     Left e -> throwError err500 {errBody = pack $ show e}
            -- where
            --     waitForNotifications con = Connection.withLibPQConnection con $ void . forever . pqFetch
            --     pqFetch pqCon = do
            --       mNotification <- PQ.notifies pqCon
            --       case mNotification of
            --         Nothing -> do
            --           mfd <- PQ.socket pqCon
            --           case mfd of
            --             Nothing  -> print "nope"
            --             Just fd -> do
            --               void $ threadWaitRead fd
            --               void $ PQ.consumeInput pqCon
            --         Just notification ->
            --             print notification
                       -- sendNotification (PQ.notifyRelname notification) (PQ.notifyExtra notification)
                       -- ServerEvent (Just $ string8 $ show $ notificationChannel notification')
                       --             Nothing
                       --             [string8 $ show $ notificationData notification']

            -- _ <- liftIO $ run (sql ("listen test")) conn
            -- return $ fromAction (\_ -> False) $ do
            --     notification <- getNotification conn
            --     liftIO $ print notification
            --     return $ case notification of
            --         Right notification' -> ServerEvent (Just $ string8 $ show $ notificationChannel notification')
            --                          Nothing
            --                          [string8 $ show $ notificationData notification']
            --         Left _ -> CloseEvent
