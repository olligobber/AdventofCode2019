import Data.Function (on)
import Data.List (sort)
import Control.Monad (guard)
import Data.Maybe (isNothing)

neverDecrease :: Int -> Bool
neverDecrease i = ((==) `on` ($ show i)) sort id

consecutive :: Int -> Bool
consecutive i = isNothing $
    foldl1
        (\x y -> if x == y then Nothing else x >> y)
        (Just <$> show i)

allNums :: [Int]
allNums = [357253..892942]

allCandidates :: [Int]
allCandidates = do
    x <- allNums
    guard $ neverDecrease x
    guard $ consecutive x
    return x

totalCandidates :: Int
totalCandidates = sum $ 1 <$ allCandidates

main :: IO ()
main = print totalCandidates
-- main = print allCandidates
