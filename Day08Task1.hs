import Data.Semigroup (Sum(..))

splitsAt :: Int -> [a] -> [[a]]
splitsAt _ [] = []
splitsAt i x = start:splitsAt i end where
    (start,end) = splitAt i x

countChar :: Char -> (Sum Int, Sum Int, Sum Int)
countChar '0' = (Sum 1, Sum 0, Sum 0)
countChar '1' = (Sum 0, Sum 1, Sum 0)
countChar '2' = (Sum 0, Sum 0, Sum 1)
countChar _ = mempty

counts :: String -> (Int, Int)
counts x = (zeroes, ones * twos) where
    (Sum zeroes, Sum ones, Sum twos) = foldMap countChar x

main :: IO ()
main = interact $ show . snd . minimum . fmap counts . concatMap (splitsAt 150) . words
-- main = interact $ show . fmap length . splitsAt 150
-- main = interact $ show . length
