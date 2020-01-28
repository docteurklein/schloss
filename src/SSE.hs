module SSE(sse) where

import Data.ByteString.Builder (string8)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Monad
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppChan)
import Network.Wai.Handler.Warp (runEnv)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Notification


writeToChan :: Chan ServerEvent -> Connection -> IO ()
writeToChan chan conn = forever $ do
    notification <- getNotification conn
    writeChan chan $ ServerEvent (Just $ string8 $ show $ notification)
                                 Nothing
                                 [string8 $ show $ notification]

sse :: IO ()
sse = do
    conn <- connectPostgreSQL ""
    execute_ conn "listen foo"
    chan <- newChan
    _ <- forkIO $ (writeToChan chan conn)
    runEnv 8080 $ eventSourceAppChan chan
