---- 3
-- a)
instance (Num a, Num b, Num c) => Num (a, b, c) where
    (+) (a,b,c) (x,y,z) = (a+x,b+y,c+z)
    signum (a,b,c) = (signum a, signum b, signum c)

-- b)
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = concat $ foldr (filterN . f) [] 
  where 
    filterN (Just x) = [x]
    filterN Nothing = []

mapMaybeEric :: (a -> Maybe b) -> [a] -> [b]
mapMaybeEric _ [] = []
mapMaybeEric f l = map d (filter c (map f l))
  where
    c Nothing = False
    c _ = True
    d (Just a) = a

-- c)
lookup' :: Eq k => k -> [(k, a)] -> Maybe a
lookup' k xs = foldl (\x (y1,y2) -> if k == y1 && x == Nothing then Just y2 else x) Nothing xs

-- d)
lookupTable = zip ['a'..'z'] (reverse ['a'..'z'])

revAlphabet :: String -> String
revAlphabet = mapMaybe (\x -> lookup' x lookupTable)


---- 4)
type Feature = String
type Min = Int
type Max = Int

data FeatureTree =
  Feature Feature [FeatureTree] |
  NoGroup ([FeatureTree], [FeatureTree]) |
  OrGroup Min Max [FeatureTree] |
  AltGroup [FeatureTree]
    deriving (Eq, Ord, Show)

-- a)
foldFT :: (Feature -> [a] -> a) -> ([a] -> [a] -> a) -> (Min -> Max -> [a] -> a) -> ([a] -> a) -> FeatureTree -> a
foldFT ff fng fog fag (Feature feature xs) = ff feature (map (foldFT ff fng fog fag) xs)
foldFT ff fng fog fag (NoGroup (xs1, xs2)) = fng (map (foldFT ff fng fog fag) xs1) (map (foldFT ff fng fog fag) xs2)
foldFT ff fng fog fag (OrGroup fmin fmax xs) = fog fmin fmax (map (foldFT ff fng fog fag) xs)
foldFT ff fng fog fag (AltGroup xs) = fag (map (foldFT ff fng fog fag) xs)

-- b)
featureList :: FeatureTree -> [String]
featureList ft = foldFT (\x y -> x ++ y) (++) (\_ _ x -> x) (id) ft

-- c)
relaxFT :: FeatureTree -> FeatureTree
relaxFT = foldFT (\x y -> (Feature x y)) (\x y -> (NoGroup ([], x ++ y))) (\x y xs -> (OrGroup x y xs)) (\xs -> (OrGroup 1 (length xs) xs))

-- d)
countFT :: FeatureTree -> (Int, Int, Int)
countFT = foldFT (\_ y -> y) (\x y -> (length (filter (filterFeature) x), length (filter (filterFeature) y), length (filter (filterFeature) x) + length (filter (filterFeature) y) )) (\_ _ x -> x) (id)
  where
    filterFeature (Feature _ _) = True
    filterFeature _ = False




















