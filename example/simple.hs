
import Data.UUID;

newtype FirstName = FirstName Text
newtype LastName = LastName Text
newtype UserId = UserId UUID

data Command = AddUser (Maybe UserId) LastName FirstName
             | BanUser UserId


data Event = UserAdded UserId LastName FirstName
           | UserBanned UserId

