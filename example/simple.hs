
import Data.UUID;

newtype FirstName = FirstName Text
newtype LastName = LastName Text
newtype UserId = UserId UUID

data Command = AddUser (Maybe UserId) LastName FirstName
             | BanUser UserId


data Event = UserAdded UserId LastName FirstName
           | UserBanned UserId

-- data MessageInput' a = MessageInput {
--     message_id :: Validatable a Text (Maybe UUID)
--   , payload :: Validatable a Text Value
--   , topic :: Validatable a Text Text
-- } -- deriving stock    (Show, GHC.Generic)
--   -- deriving anyclass (SOP.Generic, FromJSON, ToJSON, HasParams)
-- 
-- type MessageInput = MessageInput' Identity
-- deriving instance Show MessageInput
-- deriving via MessageInput instance FromJSON MessageInput
-- deriving via MessageInput instance ToJSON MessageInput
-- deriving via MessageInput instance HasParams MessageInput
-- deriving instance (Constraints (MessageInput' f) HasParams) => HasParams (MessageInput' f)

-- lensesForMessageInput :: LensesOf MessageInput MessageInput 0
-- lensesForMessageInput = makeLensesOf
-- 
-- topicLens :: Lens MessageInput MessageInput Text Text
-- topicLens = getLensOf $ topic lensesForMessageInput
-- 
-- nonempty' :: Monad m => Text -> ExceptT Text m Text
-- nonempty' t = if Text.null t
--   then throwE "can't be empty"
--   else pure t
-- 
-- messageInputValidator :: Monad m => Validator MessageInput m MessageInputError
-- messageInputValidator = MessageInput
--   <$> skip
--   <*> skip
--   <*> check topic nonempty'
-- 
-- type MessageInputError = MessageInput' Validate
-- deriving instance Show MessageInputError
-- deriving via MessageInputError instance ToJSON MessageInputError
-- data Receptacle (a :: Event) :: * where
--   Vase :: Receptacle 'Small
--   Glass :: Receptacle 'Small
--   Barrel :: Receptacle 'Big
-- 
-- deriving instance Show (Receptacle a)
-- 
-- instance (c (f 'Big), c (f 'Small)) => Dict1 c f where
--   dict1 = x -> case x of
--     SBig -> Dict
--     SSmall -> Dict
