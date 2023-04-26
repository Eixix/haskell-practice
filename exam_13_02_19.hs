intersperse :: a -> [a] -> [a]
intersperse t xs = init $ foldl (\x y -> x ++  [y] ++ [t]) [] xs

intercalate :: [a] -> [[a]] -> [a]
intercalate t xs = concat $ intersperse t xs

data Bit = Zero | One deriving Show

xor :: Bit -> Bit -> Bit
xor Zero Zero = Zero
xor One One = Zero
xor _ _ = One

instance Eq Bit where
  (==) Zero Zero = True
  (==) One One = True
  (==) _ _ = False

  (/=) x y = not ((==) x y)


data RegEx a = Seq [RegEx a] | Or [RegEx a] | AnySymbol | Symbol a

foldRE :: ([a] -> a) -> ([a] -> a) -> a -> (b -> a) -> RegEx b -> a
foldRE fseq for fas fs (Seq xs) = fseq (map (foldRE fseq for fas fs) xs)
foldRE fseq for fas fs (Or xs) = for (map (foldRE fseq for fas fs) xs)
foldRE fseq for fas fs AnySymbol = fas
foldRE fseq for fas fs (Symbol a) = fs a


showRegEx :: RegEx Char -> String
showRegEx = foldRE concat (\xs -> "(" ++ intercalate "|" xs ++ ")") "." show 

