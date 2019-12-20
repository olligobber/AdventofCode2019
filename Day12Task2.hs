import Data.Maybe (fromJust)

data Moon = Moon { position :: Integer, velocity :: Integer } deriving Eq

offset :: Integer -> Integer -> Integer
offset a b = case compare a b of
    LT -> 1
    EQ -> 0
    GT -> -1

moveMoon :: Moon -> Moon
moveMoon m = Moon (position m + velocity m) (velocity m)

gravityMoon :: [Moon] -> Moon -> Moon
gravityMoon moons m = Moon (position m) (velocity m + vchange) where
    vchange = sum $ (offset (position m) . position) <$> moons

updateMoons :: [Moon] -> [Moon]
updateMoons moons = moveMoon . gravityMoon moons <$> moons

updateUntilRepeat :: [Moon] -> Integer
updateUntilRepeat start = fromJust $ lookup start $ drop 1 $ flip zip [0..] $ iterate updateMoons start

startState :: [(Integer, Integer, Integer)]
-- startState = [(-1,0,2), (2,-10,-7), (4,-8,8), (3,5,-1)] -- Test 1
startState = [(6,10,10),(-9,3,17),(9,-4,14),(4,14,4)]

startX :: [Moon]
startX = (\(x,_,_) -> Moon x 0) <$> startState

startY :: [Moon]
startY = (\(_,y,_) -> Moon y 0) <$> startState

startZ :: [Moon]
startZ = (\(_,_,z) -> Moon z 0) <$> startState

repeatPoint :: Integer
repeatPoint = foldl1 lcm $ updateUntilRepeat <$> [startX, startY, startZ]

main :: IO ()
main = print repeatPoint
