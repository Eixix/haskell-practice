data Writer a = Out String a 
    deriving Show

instance Functor Writer where
    fmap f (Out s x) =  Out s (f x)

instance Applicative Writer where
    pure = Out ""
    Out s1 f <*> Out s2 x = Out (s1++s2) (f x)

instance Monad Writer where
  return           = pure
  Out s1 x >>= f   = let Out s2 y = f x
                     in Out (s1 ++ s2) y

write :: String -> Writer ()
write s = Out s ()

data Exp = Con Int | Div Exp Exp | Mul Exp Exp

evalW :: Exp -> Writer Int
evalW (Con n) = write (show n) >> return n
evalW (Div e1 e2) = evalW e1 >>= \x1 ->
  evalW e2 >>= \x2 ->
    let x = x1 `div` x2 in
      write (show x1 ++ "/" ++ show x2 ++ "=" ++ show x) >> return x

evalWDo :: Exp -> Writer Int
evalWDo (Con n) = write (show n) >> return n
evalWDo (Div e1 e2) = do
  x1 <- evalWDo e1
  x2 <- evalWDo e2
  let x = x1 `div` x2
  write (show x1 ++ "/" ++ show x2 ++ "=" ++ show x) 
  return x

execWriter :: Writer () -> String
execWriter (Out s ()) = s

