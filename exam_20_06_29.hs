inits :: [a] -> [[a]]
inits [] = [[]]
inits xs = inits (init xs) ++ [xs]

findIndeces :: (a -> Bool) -> [a] -> [Int]
findIndeces f xs = [n | n <- [0..(length xs) - 1], f (xs !! n)]

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f x = case f x of
  Nothing -> []
  Just (x1, y) -> x1 : unfoldr f y 
    
reverse' :: [a] -> [a]
reverse' = unfoldr (\x -> if null x then Nothing else Just(last x, init x))


