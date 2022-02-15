-- c)
join :: Show s => String -> [s] -> String
join _ [] = ""
join sep (x:xs) = show x ++ sep ++ join sep xs


joinEric :: Show s => String -> [s] -> String
joinEric _ [] = ""
joinEric s l@(x:xs)
  | length l == 1 = show x
  | otherwise = show x ++ s ++ joinEric s xs


-- d)
evenOrOdd x = if even x then Left x else Right x

mapEither :: (a -> Either b c) -> [a] -> ([b], [c])
mapEither f xs = (left, right)
  where
    left = foldl (\x y -> x ++ filterL y) [] (map f xs)
    filterL (Left x) = [x]
    filterL _ = []
    right = foldl (\x y -> x ++ filterR y) [] (map f xs)
    filterR (Right x) = [x]
    filterR _ = []
