import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

type Username = String
type Secret = String
type Reason = String
type Password = String

newtype User = User Username deriving (Eq, Show, Ord)

data Action = LogIn User | LogOut User deriving (Show)
data Perform = Try Action | Perform Action | Fail Action Reason

type UserRegistry = [(User, Secret)]

type ActionLog = [Perform]
data UserState = LoggedOut | LoggedIn User deriving (Show)

type LoginSystem a = ReaderT UserRegistry (WriterT ActionLog (StateT UserState IO)) a

sysLog :: Perform -> LoginSystem ()
sysLog p = tell [p]

getUser :: User -> LoginSystem (Maybe (User, Secret))
getUser user = do
  userRegistry <- ask
  case lookup user userRegistry of
    Nothing -> pure Nothing
    Just secret -> pure $ Just (user, secret)

failLogin :: Reason -> User -> LoginSystem ()
failLogin reason user = do
  liftIO $ putStrLn reason

succeedLogin :: User -> LoginSystem ()
succeedLogin user = do
  sysLog $ Perform (LogIn user)
  put (LoggedIn user)
