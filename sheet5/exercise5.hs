type Feature = String

type Min = Int

type Max = Int

data FeatureDiagram
  = Feature Feature [FeatureDiagram]
  | NoGroup ([FeatureDiagram], [FeatureDiagram])
  | OrGroup Min Max [FeatureDiagram]
  | AltGroup [FeatureDiagram]
  deriving (Eq, Show)

bspFD :: FeatureDiagram
bspFD =
  Feature
    "Car"
    [ NoGroup
        ( [ Feature "Carbody" [],
            Feature
              "Gearbox"
              [ AltGroup
                  [ Feature "Manual" [],
                    Feature "Automatic" []
                  ]
              ]
          ],
          [ Feature
              "Radio"
              [ NoGroup
                  ( [],
                    [ Feature
                        "Ports"
                        [ OrGroup
                            1
                            3
                            [ Feature "USB" [],
                              Feature "CD" [],
                              Feature "Bluetooth" []
                            ]
                        ],
                      Feature "Navi" []
                    ]
                  )
              ]
          ]
        )
    ]

bspFDsmall :: FeatureDiagram
bspFDsmall = OrGroup 1 1 [Feature "Hello" [Feature "World" []]]

foldFD :: (Feature -> [b] -> b) -> ([b] -> [b] -> b) -> (Min -> Max -> [b] -> b) -> ([b] -> b) -> FeatureDiagram -> b
foldFD ff ngf ogf agf fd@(Feature feature fds) = ff feature (map (foldFD ff ngf ogf agf) fds)
foldFD ff ngf ogf agf fd@(OrGroup fdmin fdmax fds) = ogf fdmin fdmax (map (foldFD ff ngf ogf agf) fds)
foldFD ff ngf ogf agf fd@(AltGroup fds) = agf (map (foldFD ff ngf ogf agf) fds)
foldFD ff ngf ogf agf fd@(NoGroup (fds1, fds2)) = ngf (map (foldFD ff ngf ogf agf) fds1) (map (foldFD ff ngf ogf agf) fds2)

featureList :: FeatureDiagram -> [Feature]
featureList = foldFD (\x y -> x : concat y) (\x y -> concat x ++ concat y) (\_ _ z -> concat z) concat