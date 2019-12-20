import Data.Semigroup (Endo(..), stimes)

getMask :: Int -> [Int]
getMask i = tail $ cycle $ concatMap (replicate i) [0,1,0,-1]

mask :: [Int] -> [Int] -> Int
mask a b = sum $ zipWith (*) a b

normalise :: Int -> Int
normalise = flip mod 10 . abs

doPhase :: [Int] -> [Int]
doPhase x = normalise . mask x . getMask <$> [1..length x]

doPhaseTimes :: Int -> [Int] -> [Int]
doPhaseTimes i = appEndo $ stimes i $ Endo doPhase

main :: IO ()
main = interact $ (++"\n") . concatMap show . doPhaseTimes 100 . fmap (read . return) . head . words
