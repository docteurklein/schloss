module Main where

import Data.Text (Text)
import PostgreSQL.Binary.Data (Vector)
import Data.ByteString.Builder (string8, toLazyByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Network.Wai.Handler.Warp (runEnv)
import Network.Wai.EventSource (ServerEvent(..))
import Network.Wai.EventSource.EventStream (eventToBuilder)
import Hasql.Session ()
import Hasql.Statement (Statement)
import Hasql.Notification (notificationChannel, notificationData, getNotification)
import qualified Hasql.Session as Session
import qualified Hasql.Connection as Connection
import qualified Hasql.TH as TH
import Hasql.Pool (Pool, use, acquire)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant (
        MimeRender(..)
      , Accept(..)
      , ServerT
      , Handler
      , Application
      , Proxy(..)
      , Header
      , QueryParam
      , Capture
      , StreamGet
      , NewlineFraming
      , JSON
      , Get
      , Post
      , ReqBody
      , NoContent(..)
      , ServerError(..)
      , err500
      , throwError
      , serve
      , hoistServer
      , (:>)
      , (:<|>)(..)
      )
import Servant.API.Stream (SourceIO)
import Network.HTTP.Media ((//))
import Servant.Types.SourceT (fromAction)
import System.Log.FastLogger (ToLogStr(..), LoggerSet, defaultBufSize, newStdoutLoggerSet, pushLogStrLn)


type Api = ReqBody '[JSON] Message :> Post '[JSON] NoContent
      :<|> Get '[JSON] (Vector Text)
      :<|> "sse"
           :> Capture "topic" Text
           :> QueryParam "since" Text
           :> Header "Last-Event-Id" Text
           :> StreamGet NewlineFraming EventStream (SourceIO ServerEvent)

data EventStream

instance Accept EventStream where
    contentType _ = "text" // "event-stream"

instance MimeRender EventStream ServerEvent where
  mimeRender _ = maybe "" toLazyByteString . eventToBuilder

data Message = Message {
    content :: Text
  , topic :: Text
} deriving (Show, Generic, FromJSON, ToJSON)

data Config = Config {
    logger :: LoggerSet
  , pool :: Pool
  , conn :: Connection.Connection
}

main :: IO ()
main = do
    pool <- acquire (1, 1, "")
    Right conn <- Connection.acquire ""
    _ <- use pool $ Session.sql initSchema
    logger <- newStdoutLoggerSet defaultBufSize
    runEnv 8888 $ mkApp $ Config logger pool conn

api :: Proxy Api
api = Proxy

mkApp :: Config -> Application
mkApp config = serve api $ hoistServer api (flip runReaderT config) server

initSchema = [TH.uncheckedSql|
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

type AppM = ReaderT Config Handler

server :: ServerT Api AppM
server = postMessage :<|> getMessages :<|> sse
    where
        postMessage :: Message -> AppM NoContent
        postMessage msg = do
            logset <- asks logger
            liftIO $ pushLogStrLn logset $ toLogStr $ ("post message: " ++ show msg)
            pool <- asks pool
            result <- liftIO $ use pool $ Session.statement (content msg, topic msg) insertMessage
            case result of
                Right _ -> pure NoContent
                Left e -> throwError err500 {errBody = pack $ show e}
            where
                insertMessage = [TH.resultlessStatement|insert into message (content, topic) values ($1::text, $2::text)|]

        getMessages :: AppM (Vector Text)
        getMessages = do
            logset <- asks logger
            liftIO $ pushLogStrLn logset $ "get messages"
            pool <- asks pool
            result <- liftIO $ use pool $ Session.statement () selectMessages
            case result of
                Right messages -> pure messages
                Left e -> throwError err500 {errBody = pack $ show e}
            where
                selectMessages :: Statement () (Vector (Text))
                selectMessages = [TH.vectorStatement|select content::text from message|]

        sse :: Text -> Maybe Text -> Maybe Text -> AppM (SourceIO ServerEvent)
        sse topic _since _lastEventId = do
            conn <- asks conn
            _ <- liftIO $ Session.run (Session.sql ("listen test")) conn
            return $ fromAction (\_ -> False) $ do
                notification <- getNotification conn
                liftIO $ putStrLn $ show notification
                return $ case notification of
                    Right notification' -> ServerEvent (Just $ string8 $ show $ notificationChannel notification')
                                     Nothing
                                     [string8 $ show $ notificationData notification']
                    Left _ -> CloseEvent
