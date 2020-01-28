module Main where

import Data.UUID
import Data.UUID.V4 (nextRandom)
import Data.Text
import qualified Schloss (Command, Event, Actor)
import SSE

newtype FirstName = FirstName Text
    deriving (Eq, Ord, Show)

newtype LastName = LastName Text
    deriving (Eq, Ord, Show)

newtype UserId = UserId UUID
    deriving (Eq, Ord, Show)

data Command
    = AddUser (Maybe UUID) FirstName LastName
    | BanUser UserId
    deriving (Eq, Ord, Show)

--type instance Schloss.Command = Command

data Event
    = UserAdded UserId FirstName LastName
    | UserBanned UserId
    deriving (Eq, Ord, Show)

--type instance Schloss.Event = Event


main :: IO ()
-- main = do
--     userId <- nextRandom
--     putStrLn $ show $ AddUser (Just userId) (FirstName "Florian") (LastName "Klein")
main = sse
