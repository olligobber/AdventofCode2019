splitsAt :: Int -> [a] -> [[a]]
splitsAt _ [] = []
splitsAt i x = start:splitsAt i end where
    (start,end) = splitAt i x

combine :: Char -> Char -> Char
combine '2' x = x
combine x _ = x

prettyPrint :: Char -> Char
prettyPrint '1' = '#'
prettyPrint _ = ' '

main :: IO ()
main = interact $ unlines . splitsAt 25 . fmap prettyPrint . foldl1 (zipWith combine) . concatMap (splitsAt 150) . words
