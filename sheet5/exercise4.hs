type Width = Int

type Height = Int

data Building
  = Rectangle Width Height Building
  | Tip Width Height
  | Split Building Building
  deriving (Eq, Show)

bspBuilding :: Building
bspBuilding =
  Rectangle
    50
    20
    ( Split
        ( Rectangle
            20
            15
            ( Split
                ( Rectangle
                    10
                    20
                    ( Rectangle
                        8
                        18
                        (Tip 8 14)
                    )
                )
                ( Rectangle
                    8
                    17
                    (Tip 8 14)
                )
            )
        )
        ( Rectangle
            20
            15
            ( Split
                ( Rectangle
                    8
                    17
                    (Tip 8 14)
                )
                ( Rectangle
                    10
                    20
                    ( Split
                        (Tip 5 17)
                        (Tip 5 17)
                    )
                )
            )
        )
    )

foldBuilding :: (n -> Building -> n) -> (n -> n -> n) -> n -> Building -> n
foldBuilding f g n b@(Rectangle _ _ b2) = f (foldBuilding f g n b2) b
foldBuilding f g n (Split b1 b2) = g (foldBuilding f g n b1) (foldBuilding f g n b2)
foldBuilding f _ n b = f n b

maxHeight :: Building -> Height
maxHeight = foldBuilding getHeight max 0
  where
    getHeight x (Rectangle _ h _) = x + h
    getHeight x (Tip _ h) = x + h
    getHeight x _ = x

numParts :: Building -> Int
numParts = foldBuilding getParts (+) 0
  where
    getParts x _ = x + 1

numPeaks :: Building -> Int
numPeaks = foldBuilding getPeaks (+) 0
  where
    getPeaks x (Tip _ _) = x + 1
    getPeaks x _ = x

wellformed :: Building -> Bool
wellformed = foldBuilding getWellformed (&&) True
  where
    getChildWidth (Tip w _) = w
    getChildWidth (Rectangle w _ _) = w
    getChildWidth (Split b1 b2) = getChildWidth b1 + getChildWidth b2
    getWellformed x (Rectangle w _ b1) = x && w >= getChildWidth b1
    getWellformed x _ = x
