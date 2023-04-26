data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a) deriving (Eq, Show)

bsp = Node "Keinentschlus"
  (Node "Antivirus"
    Empty
    (Leaf "Darmverschlus")
  )
  (Node "Redkeinstus"
    (Leaf "Nichtsalsverdrus")
    (Leaf "Spiritus")
  )

foldTree :: b -> (a -> b) -> (a -> b -> b -> b) -> Tree a -> b
foldTree fe fl fn Empty = fe
foldTree fe fl fn (Leaf a) = fl a
foldTree fe fl fn (Node a t1 t2) = fn a (foldTree fe fl fn t1) (foldTree fe fl fn t2)

inorder :: Tree a -> [a]
inorder = foldTree [] (:[]) (\x t1 t2 -> t1 ++ [x] ++ t2)

lookupHeight :: (Eq a) => a -> Tree a -> Maybe Int
lookupHeight search = foldTree Nothing (\x -> if x == search then Just 1 else Nothing) bigFunc

  where
    bigFunc y Nothing Nothing = if y == search then Just 1 else Nothing
    bigFunc _ (Just x) _ = Just (x+1)
    bigFunc _ _ (Just x) = Just (x+1)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node a t1 t2) = Node (f a) (fmap f t1) (fmap f t2)

dataLength :: Tree [a] -> Tree Int
dataLength t = length <$> t

data Labyrinth = Cell Int (Direction -> Maybe Labyrinth) | Exit

data Direction = GoLeft | GoRight | GoUp | GoDown deriving (Eq, Enum, Show)

bspLabyrinth :: Labyrinth
bspLabyrinth = cell1
  where 
    cell1 = Cell 1 (lookUpFunction [(GoDown, cell4)])
    cell2 = Cell 2 (lookUpFunction [(GoDown, cell5)])
    cell3 = Cell 3 (lookUpFunction [(GoDown, cell6), (GoRight, Exit)])
    cell4 = Cell 4 (lookUpFunction [(GoUp, cell1), (GoRight, cell5)])
    cell5 = Cell 5 (lookUpFunction [(GoLeft, cell4), (GoRight, cell2), (GoRight, cell6)])
    cell6 = Cell 6 (lookUpFunction [(GoLeft, cell5), (GoUp, cell3)])

lookUpFunction :: [(Direction, Labyrinth)] -> (Direction -> Maybe Labyrinth)
lookUpFunction [] = const Nothing
lookUpFunction ((d,l):xs) = \x -> if x == d then Just l else Nothing

