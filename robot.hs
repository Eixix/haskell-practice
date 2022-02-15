import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

-- Bewegungsanweisungen (North, East, South, West)
data Dir = N | E | S | W

-- Durchfuehren der Bewegung
move :: (Int, Int) -> Dir -> (Int, Int)
move (x,y) N = (x,y+1)
move (x,y) S = (x,y-1)
move (x,y) E = (x+1,y)
move (x,y) W = (x-1,y)

tryMove :: (Int,Int) ->               -- aktuelle Position
                 [(Int,Int)] ->              -- Liste der Hindernisse
                 Dir ->                        -- Bewegungsanweisung
                 Maybe (Int,Int)        -- neue Position (oder Nothing)
tryMove curPos obst dir =
  let
    newPos = move curPos dir     -- ein Schritt in die gewuenschte Richtung
  in
    if newPos `elem` obst             -- wenn neue Position ein Hindernis ist..
      then Nothing                         -- .. ist Ergebnis Nothing
      else Just newPos                   -- .. sonst die neue Position

moveRobot :: (Int, Int) -> [(Int,Int)] -> [Dir] -> Maybe (Int,Int)
moveRobot curPos _ [] = Just curPos
moveRobot curPos obst (dir:rest) =
  case tryMove curPos obst dir of
    Nothing -> Nothing
    Just newPos -> moveRobot newPos obst rest


moveRobotM :: (Int, Int) -> [(Int,Int)] -> [Dir] -> Maybe (Int,Int)
moveRobotM curPos _ [] = Just curPos
moveRobotM curPos obst dirs = do
  x <- tryMove curPos obst (head dirs)
  moveRobotM x obst (tail dirs)

moveRobotW :: (Int, Int) -> [(Int,Int)] -> [Dir] -> Writer String (Maybe (Int,Int))
moveRobotW curPos _ [] = tell (show curPos) >> return (Just curPos)
moveRobotW curPos obst dirs = do
  tell (show curPos)
  case tryMove curPos obst (head dirs) of
    Nothing -> return Nothing
    Just newPos -> moveRobotW newPos obst (tail dirs)

moveRobotR :: (Int, Int) -> [Dir] -> Reader [(Int, Int)] (Maybe (Int, Int))
moveRobotR curPos [] = return (Just curPos)
moveRobotR curPos dirs = do
  obst <- ask
  case tryMove curPos obst (head dirs) of
    Nothing -> return Nothing
    Just newPos -> moveRobotR newPos (tail dirs)

foldM' :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldM' f z [] = return z
foldM' f z (x:xs) = do
  z1 <- f z x
  foldM' f z1 xs

type RobotMonad a = ReaderT [a] (MaybeT (Writer [a])) a

tryMoveRWM :: (Int, Int) -> Dir -> RobotMonad (Int,Int) 
tryMoveRWM curPos dir = do
  tell [curPos]
  obst <- ask
  let newPos = move curPos dir
  if newPos `elem` obst 
    then error "Error"
  else
    return newPos

moveRobotRWM :: (Int, Int) -> [Dir] -> RobotMonad (Int, Int)
moveRobotRWM curPos [] = return curPos
moveRobotRWM curPos dirs = do
  x <- tryMoveRWM curPos (head dirs)
  moveRobotRWM x (tail dirs)

runRobotRWM :: RobotMonad a -> [a] -> (Maybe a,[a])
runRobotRWM m a = runWriter (runMaybeT (runReaderT m a))
