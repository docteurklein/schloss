module SSE(sse) where

import Data.ByteString.Builder (string8)
import Data.Text(Text, unpack)
import Data.String
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Monad
import Network.Wai (Application, pathInfo, responseLBS)
import Network.HTTP.Types.Status (status400)
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppChan, eventSourceAppIO)
import Network.Wai.Handler.Warp (runEnv)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Notification


writeToChan :: Chan ServerEvent -> Connection -> IO ()
writeToChan chan conn = forever $ do
    notification <- getNotification conn
    writeChan chan $ ServerEvent (Just $ string8 $ show $ notification)
                                 Nothing
                                 [string8 $ show $ notification]

eventIO :: String -> IO ServerEvent
eventIO topic = do
    conn <- connectPostgreSQL ""
    putStrLn topic
    execute conn "listen ?" (topic)
    chan <- newChan
    _ <- forkIO $ (writeToChan chan conn)
    readChan chan

app :: Application
app req respond =
    case pathInfo req of
        [topic] -> eventSourceAppIO (eventIO $ unpack topic) req respond
        _ -> respond $ responseLBS status400 [] "/:topic"

sse :: IO ()
sse = runEnv 8080 app
