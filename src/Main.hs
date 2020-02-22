module Main where

import Data.Text (Text)
import Data.Int
import PostgreSQL.Binary.Data
import Data.ByteString.Builder (string8)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.IO.Class
import Control.Monad.Reader
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppIO)
import Network.Wai.Handler.Warp (runEnv)
import Hasql.Session (Session)
import Hasql.Statement (Statement)
import Hasql.Notification
import qualified Hasql.Session as Session
import qualified Hasql.Connection as Connection
import qualified Hasql.TH as TH
import Hasql.Pool
import Data.Aeson
import GHC.Generics
import Servant
import System.Log.FastLogger ( ToLogStr(..)
                             , LoggerSet
                             , defaultBufSize
                             , newStdoutLoggerSet
                             , pushLogStrLn )


type API = ReqBody '[JSON] Message :> Post '[JSON] NoContent
      :<|> Get '[JSON] (Vector Text)
      :<|> "sse"
           :> Capture "topic" Text
           :> QueryParam "since" Text
           :> Header "Last-Event-Id" Text
           :> Raw

api :: Proxy API
api = Proxy

data Message = Message { content :: Text }
    deriving (Show, Generic, FromJSON, ToJSON)

data AppCtx = AppCtx {
    logger :: LoggerSet
  , pool :: Pool
}

main :: IO ()
main = do
    pool <- acquire (1, 1, "")
    use pool $ Session.sql initSchema
    logger <- newStdoutLoggerSet defaultBufSize
    let ctx = AppCtx logger pool
    runEnv 8888 $ mkApp ctx

mkApp :: AppCtx -> Application
mkApp ctx = serve api $ hoistServer api (flip runReaderT ctx) server

initSchema = [TH.uncheckedSql|
    create table if not exists message (
        message_id uuid primary key default uuid_generate_v4(),
        content text not null,
        at timestamptz not null default clock_timestamp()
    )
|]

type AppM = ReaderT AppCtx Handler

server :: ServerT API AppM
server = postMessage :<|> getMessages :<|> sse
    where
        postMessage :: Message -> AppM NoContent
        postMessage msg = do
            logset <- asks logger
            liftIO $ pushLogStrLn logset $ toLogStr $ ("post message: " ++ show (content msg))
            pool <- asks pool
            result <- liftIO $ use pool $ Session.statement (content msg) insertMessage
            case result of
                Right messages -> pure NoContent
                Left e -> parseUsageError e
            where
                insertMessage = [TH.resultlessStatement|insert into message (content) values ($1::text)|]

        getMessages :: AppM (Vector Text)
        getMessages = do
            logset <- asks logger
            liftIO $ pushLogStrLn logset $ "get messages"
            pool <- asks pool
            result <- liftIO $ use pool $ Session.statement () selectMessages
            case result of
                Right messages -> pure messages
                Left e -> parseUsageError e
            where
                selectMessages :: Statement () (Vector (Text))
                selectMessages = [TH.vectorStatement|select content::text from message|]

        sse topic since lastEventId = Tagged $ \req respond -> do
            putStrLn $ show since
            putStrLn $ show lastEventId
            -- --_ <- withResource pool $ \conn -> do
            -- --    notifications <- query conn "select content from message where at <= now()" ()
            -- --    eventSourceAppIO (since notifications) req respond

            eventSourceAppIO (listen topic) req respond
            where
                listen :: Text -> IO ServerEvent
                listen topic = do
                    pool <- asks pool
                    result <- liftIO $ use pool $ Session.sql [TH.uncheckedSql|listen "test"|]
                    notification <- getNotification $ use pool
                    case result of
                        Right notification -> event notification
                        Left e -> error "nope"

                    where
                        event notification' = return $ ServerEvent (Just $ string8 $ show $ notificationChannel notification')
                                                         Nothing
                                                         [string8 $ show $ notificationData notification']

        parseUsageError e = throwError err500 {errBody = pack $ show e}

