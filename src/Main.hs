module Main where

import Data.Text (Text)
import Data.ByteString.Builder (string8)
import Data.ByteString (ByteString)
import Control.Monad.IO.Class
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppIO)
import Network.Wai.Handler.Warp (runEnv)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Notification
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types
import Data.Pool
import Data.Aeson
import GHC.Generics
import Servant
import Text.RawString.QQ

main :: IO ()
main = do
    pool <- initConnectionPool ""
    _ <- withResource pool $ \conn ->
        execute_ conn [r|
            create table if not exists message (
                message_id uuid primary key default uuid_generate_v4(),
                content text not null,
                at timestamptz not null default clock_timestamp()
            )
        |]
    runApp pool

data Message = Message { content :: Text }
    deriving (
        Show
      , Generic
      , FromJSON
      , ToJSON
    )

instance FromRow Message where
    fromRow = Message <$> field

type API = ReqBody '[JSON] Message :> Post '[JSON] NoContent
      :<|> Get '[JSON] [Message]
      :<|> "sse" :> Capture "topic" Text :> QueryParam "since" Text :> Header "Last-Event-Id" Text :> Raw

api :: Proxy API
api = Proxy

server :: Pool Connection -> Server API
server pool = postMessage :<|> getMessages :<|> sse
    where
        postMessage :: Message -> Handler NoContent
        postMessage msg = do
            _ <- liftIO . withResource pool $ \conn ->
                execute conn "insert into message (content) values (?)" (Only $ content msg)
            return NoContent

        getMessages :: Handler [Message]
        getMessages = liftIO $ withResource pool $ \conn ->
            query_ conn "select content from message"

        sse topic since lastEventId = Tagged $ \req respond -> do
            putStrLn $ show since
            putStrLn $ show lastEventId
            --_ <- withResource pool $ \conn -> do
            --    notifications <- query conn "select content from message where at <= now()" ()
            --    eventSourceAppIO (since notifications) req respond

            eventSourceAppIO (listen pool topic) req respond

        listen :: Pool Connection -> Text -> IO ServerEvent
        listen pool' topic =
            withResource pool' $ \conn -> do
                _ <- execute conn "listen ?" (Only $ Identifier topic)
                notification <- getNotification conn
                return $ ServerEvent (Just $ string8 $ show $ notificationChannel notification)
                                     Nothing
                                     [string8 $ show $ notificationData notification]


runApp :: Pool Connection -> IO ()
runApp pool = runEnv 8888 $ serve api $ server pool
initConnectionPool :: ByteString -> IO (Pool Connection)
initConnectionPool dsn =
  createPool (connectPostgreSQL dsn)
             close
             2 -- stripes
             60 -- unused connections are kept open for a minute
             10 -- max. 10 connections open per stripe
