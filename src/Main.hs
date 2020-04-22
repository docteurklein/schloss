module Main where

import Data.Vector (Vector)
import Data.ByteString.Builder (toLazyByteString)
import Data.UUID (UUID)
-- import Data.Profunctor (dimap, rmap)
import Data.Time.Clock (UTCTime)
-- import Data.UUID.V4 (nextRandom)
-- import Control.Concurrent.Async (async)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
-- import Control.Monad.Trans.Resource (ResourceT, runResourceT, allocate, release)
-- import Control.Monad (void, forever)
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
import Hasql.TH (singletonStatement, vectorStatement)
import Hasql.Generic.HasRow (HasRow, mkRow)
import Hasql.Generic.HasParams (HasParams, mkParams)
-- import qualified Hasql.Connection as Connection (acquire, withLibPQConnection)
-- import qualified Database.PostgreSQL.LibPQ as PQ
-- import Hasql.Notification (notificationChannel, notificationData, getNotification)
import Hasql.Notifications (listen, unlisten, toPgIdentifier)
import Hasql.Pool (Pool, use, acquire)
import Data.SirenJSON (Entity, Link, Action, Field)
-- import Network.URI (URI)
import Data.Aeson (FromJSON, ToJSON, encode, Value)
import Data.Valor (Validatable, Validate)
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


type Api = ReqBody '[JSON] (MessageInput Value) :> Post '[JSON] (Message Value)
      :<|> Get '[JSON, SirenJSON] (Vector (Message Value))
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

data MessageInput' a content = MessageInput' {
    message_id :: Validatable a String (Maybe UUID)
  , content :: Validatable a [String] content
  , topic :: Validatable a [String] Text
} -- deriving stock    (Show, GHC.Generic)
  -- deriving anyclass (SOP.Generic, FromJSON, ToJSON, HasParams)

type MessageInput = MessageInput' Identity
deriving instance Show (MessageInput Value)
deriving via (MessageInput Value) instance FromJSON (MessageInput Value)
deriving via (MessageInput Value) instance ToJSON (MessageInput Value)
deriving via (MessageInput Value) instance HasParams (MessageInput Value)

type MessageInputError = MessageInput' Validate
deriving instance Show (MessageInputError Value)
deriving via (MessageInputError Value) instance ToJSON (MessageInputError Value)


-- data MessageInput a = MessageInput {
--     content :: a
--   , topic :: Text
-- } deriving stock    (Show, GHC.Generic)
--   deriving anyclass (SOP.Generic, FromJSON, ToJSON, HasParams)

data Message a = Message {
    message_id :: UUID
  , content :: a
  , topic :: Text
  , at :: UTCTime
} deriving stock    (Show, GHC.Generic)
  deriving anyclass (SOP.Generic, FromJSON, ToJSON, HasRow)

data Config = Config {
    logger :: LoggerSet
  , pool :: Pool
}

main :: IO ()
main = do
    pool <- acquire (1, 1, "")
    logger <- newStdoutLoggerSet defaultBufSize
    pushLogStrLn logger $ "listening on port 8888"
    runEnv 8888 $ logStdoutDev . mkApp $ Config logger pool
    where
        api = Proxy @Api
        mkApp config = serve api $ hoistServer api (flip runReaderT config) server

type AppM = ReaderT Config Handler

server :: ServerT Api AppM
server = postMessage :<|> getMessages :<|> sse
    where
        postMessage :: MessageInput Value -> AppM (Message Value)
        postMessage input = do
            -- show $ view (field @"content") input
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
                insertMessage :: Statement (MessageInput Value) (Message Value)
                insertMessage = Statement sql encoder decoder True
                    where
                        sql = "insert into schloss.messages (payload, topic) values ($1, $2) returning *"
                        encoder = mkParams
                        decoder = singleRow mkRow

        getMessages :: AppM (Vector (Message Value))
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
                selectMessages :: Statement () (Vector (Message Value))
                selectMessages = Statement sql encoder decoder True
                    where
                        sql = "select * from schloss.messages"
                        encoder = noParams
                        decoder = rowVector mkRow
        sse :: Text -> Maybe Text -> Maybe Text -> AppM (SourceIO ServerEvent)
        sse topic _since _lastEventId = do
            return $ fromAction (\_ -> False) $ do
                threadDelay 1_000_000
                -- liftIO $ pushLogStrLn logset $ "sse"
                return CloseEvent
