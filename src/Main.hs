module Main where

import Data.Text (Text, pack)
import Data.Int
import PostgreSQL.Binary.Data
import Data.ByteString.Builder (string8)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.IO.Class
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppIO)
import Network.Wai.Handler.Warp (runEnv)
import Hasql.Session (Session)
import Hasql.Statement (Statement)
import qualified Hasql.Session as Session
import qualified Hasql.Connection as Connection
import qualified Hasql.TH as TH
import Hasql.Pool
import Data.Aeson
import GHC.Generics
import Servant
import Text.RawString.QQ

main :: IO ()
main = do
  pool <- acquire settings
  t <- use pool $ Session.sql initSchema
  runApp pool
  putStrLn $ show t
  where
    settings = (1, 1, "")

runApp :: Pool -> IO ()
runApp pool = runEnv 8888 $ serve api $ server pool

initSchema = [TH.uncheckedSql|
    create table if not exists message (
        message_id uuid primary key default uuid_generate_v4(),
        content text not null,
        at timestamptz not null default clock_timestamp()
    )
|]

data Message = Message { content :: Text }
    deriving (Show, Generic, FromJSON, ToJSON)

type API = ReqBody '[JSON] Message :> Post '[JSON] NoContent
      :<|> Get '[JSON] (Vector Text)
      -- :<|> "sse"
      --      :> Capture "topic" Text
      --      :> QueryParam "since" Text
      --      :> Header "Last-Event-Id" Text
      --      :> Raw

api :: Proxy API
api = Proxy

server :: Pool -> Server API
server pool = postMessage :<|> getMessages
              -- :<|> sse
    where
        postMessage :: Message -> Handler NoContent
        postMessage msg = do
            result <- liftIO $ use pool $ Session.statement (content msg) insertMessage
            case result of
                Right messages -> pure NoContent
                Left error -> parseUsageError error
            where
                insertMessage = [TH.resultlessStatement|insert into message (content) values ($1::text)|]

        getMessages :: Handler (Vector Text)
        getMessages = do
            result <- liftIO $ use pool $ Session.statement () selectMessages
            case result of
                Right messages -> pure messages
                Left error -> parseUsageError error

        parseUsageError _ = throwError err500 {errBody = LBS.fromStrict "error"}

selectMessages :: Statement () (Vector (Text))
selectMessages = [TH.vectorStatement|select content::text from message|]

        --sse topic since lastEventId = Tagged $ \req respond -> do
        --    putStrLn $ show since
        --    putStrLn $ show lastEventId
        --    --_ <- withResource pool $ \conn -> do
        --    --    notifications <- query conn "select content from message where at <= now()" ()
        --    --    eventSourceAppIO (since notifications) req respond

        --    eventSourceAppIO (listen pool topic) req respond

        --listen :: Pool -> Text -> IO ServerEvent
        --listen pool' topic =
        --    withResource pool' $ \conn -> do
        --        _ <- execute conn "listen ?" (Only $ Identifier topic)
        --        notification <- getNotification conn
        --        return $ ServerEvent (Just $ string8 $ show $ notificationChannel notification)
        --                             Nothing
        --                             [string8 $ show $ notificationData notification]

