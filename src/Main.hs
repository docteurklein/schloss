module Main where

import Data.ByteString.Builder (string8)
import Data.ByteString (ByteString)
import Data.String
import Data.Text (Text, unpack, pack)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.IO.Class
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppIO)
import Network.Wai.Handler.Warp (runEnv)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Notification
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types
import Data.Pool
import Network.Wai.Handler.Warp
import Data.Aeson
import GHC.Generics
import Servant

main :: IO ()
main = do
    pool <- initConnectionPool ""
    withResource pool $ \conn ->
        initDB conn
    runApp pool

type DBConnectionString = ByteString

newtype Message = Message { content :: String }
  deriving (Show, Generic)

instance FromJSON Message
instance ToJSON Message

instance FromRow Message where
    fromRow = Message <$> field

type API = ReqBody '[JSON] Message :> Post '[JSON] NoContent
      :<|> Get '[JSON] [Message]
      :<|> "sse" :> Capture "topic" Text :> Raw

api :: Proxy API
api = Proxy

initDB :: Connection -> IO ()
initDB conn = do
  _ <- execute_ conn
    "create table if not exists message (message_id uuid primary key default uuid_generate_v4(), content text not null)"
  return ()

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

        sse topic = Tagged $ \req respond -> do
            withResource pool $ \conn ->
                execute conn "listen ?" (Only $ Identifier topic)
            withResource pool $ \conn ->
                eventSourceAppIO (buildNotification conn topic) req respond

        buildNotification :: Connection -> Text -> IO ServerEvent
        buildNotification conn topic = do
            notification <- getNotification conn
            return $ ServerEvent (Just $ string8 $ show $ notificationChannel notification)
                                 Nothing
                                 [string8 $ show $ notificationData notification]


runApp :: Pool Connection -> IO ()
runApp pool = runEnv 8888 (serve api $ server pool)
initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool pooltr =
  createPool (connectPostgreSQL pooltr)
             close
             2 -- stripes
             60 -- unused connections are kept open for a minute
             10 -- max. 10 connections open per stripe
