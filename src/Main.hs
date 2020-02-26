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
import Servant.API.Stream
import Servant.Types.SourceT
import System.Log.FastLogger ( ToLogStr(..)
                             , LoggerSet
                             , defaultBufSize
                             , newStdoutLoggerSet
                             , pushLogStrLn )


type Api = ReqBody '[JSON] Message :> Post '[JSON] NoContent
      :<|> Get '[JSON] (Vector Text)
      :<|> "sse"
           :> Capture "topic" Text
           :> QueryParam "since" Text
           :> Header "Last-Event-Id" Text
           :> StreamGet NewlineFraming JSON (SourceIO Text)

api :: Proxy Api
api = Proxy

data Message = Message { content :: Text }
    deriving (Show, Generic, FromJSON, ToJSON)

data Config = Config {
    logger :: LoggerSet
  , pool :: Pool
  , conn :: Connection.Connection
}

main :: IO ()
main = do
    pool <- acquire (1, 1, "")
    Right conn <- Connection.acquire ""
    use pool $ Session.sql initSchema
    logger <- newStdoutLoggerSet defaultBufSize
    runEnv 8888 $ mkApp $ Config logger pool conn

mkApp :: Config -> Application
mkApp config = serve api $ hoistServer api (flip runReaderT config) server

initSchema = [TH.uncheckedSql|
    create table if not exists message (
        message_id uuid primary key default uuid_generate_v4(),
        content text not null,
        at timestamptz not null default clock_timestamp()
    )
|]

type AppM = ReaderT Config Handler

server :: ServerT Api AppM
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

        sse :: Text -> Maybe Text -> Maybe Text -> AppM (SourceIO Text)
        sse topic since lastEventId = do
            return $ source ["test", "test2"]

        parseUsageError e = throwError err500 {errBody = pack $ show e}

