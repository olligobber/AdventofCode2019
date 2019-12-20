import Data.Semigroup (Endo(..), stimes)

data Vector = Vector {
    x :: Integer,
    y :: Integer,
    z :: Integer
}

data Moon = Moon {
    position :: Vector,
    velocity :: Vector
}

componentwise :: (Integer -> Integer -> Integer) -> Vector -> Vector -> Vector
componentwise (#) v1 v2 = Vector (x v1 # x v2) (y v1 # y v2) (z v1 # z v2)

instance Semigroup Vector where
    (<>) = componentwise (+)

instance Monoid Vector where
    mempty = Vector 0 0 0

offset :: Vector -> Vector -> Vector
offset = componentwise simpleOffset where
    simpleOffset a b = case compare a b of
        LT -> 1
        EQ -> 0
        GT -> -1

moveMoon :: Moon -> Moon
moveMoon m = Moon (position m <> velocity m) (velocity m)

gravityMoon :: [Moon] -> Moon -> Moon
gravityMoon moons m = Moon (position m) (velocity m <> vchange) where
    vchange = foldMap (offset (position m) . position) moons

updateMoons :: [Moon] -> [Moon]
updateMoons moons = moveMoon . gravityMoon moons <$> moons

updateSystem :: Integer -> [Moon] -> [Moon]
updateSystem i = appEndo $ stimes i $ Endo updateMoons

vectorEnergy :: Vector -> Integer
vectorEnergy v = abs (x v) + abs (y v) + abs (z v)

moonEnergy :: Moon -> Integer
moonEnergy m = vectorEnergy (position m) * vectorEnergy (velocity m)

systemEnergy :: [Moon] -> Integer
systemEnergy = sum . fmap moonEnergy

startState :: [Moon]
startState = do
    (xp, yp, zp) <- [(6,10,10),(-9,3,17),(9,-4,14),(4,14,4)]
    return $ Moon (Vector xp yp zp) mempty

main :: IO ()
main = print $ systemEnergy $ updateSystem 1000 $ startState
