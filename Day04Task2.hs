import Data.Function (on)
import Data.List (sort, elem, group, length)
import Control.Monad (guard)

neverDecrease :: Int -> Bool
neverDecrease i = ((==) `on` ($ show i)) sort id

consecutive :: Int -> Bool
consecutive = elem 2 . fmap length . group . show

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
