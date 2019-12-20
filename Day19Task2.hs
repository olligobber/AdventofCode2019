import IntCode
import Data.List (elemIndex)

boxSize :: Num a => a
boxSize = 100

scanCoord :: [Integer] -> Integer -> Integer -> Bool
scanCoord intcode x y = Left 1 ==
    runSimpleRead [x,y] (const . Left) (\(a:as) -> (as,a)) intcode

scanSquare :: [Integer] -> Integer -> String
scanSquare intcode m = unlines $ do
    y <- [0..m]
    l <- return $ do
        x <- [0..m]
        if scanCoord intcode x y then
            "#"
        else
            "."
    return $ l ++ show y

updateCount :: Int -> Bool -> Int
updateCount _ False = 0
updateCount i True = i + 1

data Scanner = Scanner {
    leftX :: Integer,
    rightX :: Integer,
    y :: Integer,
    counts :: [Int] -- Number of filled cells above (leftX, y)...(rightX, y)
} deriving Show

updateScanner :: [Integer] -> Scanner -> Scanner
updateScanner intcode s
    | scanCoord intcode (rightX s + 1) (y s) =
        Scanner (leftX s) (rightX s + 1) (y s) (counts s ++ [1])
    | not (scanCoord intcode (leftX s) (y s + 1)) =
        Scanner (leftX s + 1) (rightX s) (y s) (tail $ counts s)
    | otherwise =
        Scanner (leftX s) (rightX s) (y s + 1) ((+1) <$> counts s)

startScanner :: Scanner
startScanner = Scanner 5 5 6 [1]
-- Beam is discontinuous before this point

anyBigBox :: [Int] -> Maybe Int
anyBigBox = elemIndex boxSize . tail . scanl updateCount 0 . fmap (>= boxSize)

-- findBigBoxIO :: [Integer] -> Scanner -> IO (Integer, Integer)
-- findBigBoxIO intcode s = case anyBigBox $ counts s of
--     Just x -> do
--         putStrLn "Program complete"
--         return (rightX s - toInteger x, y s + 1 - boxSize)
--     Nothing -> do
--         putStrLn $
--             "Nothing here, currently looking near " ++
--             show (rightX s, y s) ++
--             " with max width and height " ++
--             show (length $ counts s, foldl max 0 $ counts s)
--         findBigBoxIO intcode $ updateScanner intcode s
--
-- getAnswerIO :: [Integer] -> IO ()
-- getAnswerIO intcode = do
--     (x,y) <- findBigBoxIO intcode startScanner
--     print $ x * (10^4) + y

findBigBox :: [Integer] -> Scanner -> (Integer, Integer)
findBigBox intcode s = case anyBigBox $ counts s of
    Just x -> (leftX s + toInteger x + 1 - boxSize, y s + 1 - boxSize)
    Nothing -> findBigBox intcode $ updateScanner intcode s

getAnswer :: [Integer] -> Integer
getAnswer intcode = x * (10^4) + ay where
    (x,ay) = findBigBox intcode startScanner

main :: IO ()
-- main = readFile "Day19Input" >>= getAnswerIO . readIntCode
main = readFile "Day19Input" >>= print . getAnswer . readIntCode

-- Debug: print out the starting square
-- main = readFile "Day19Input" >>= putStrLn . flip scanSquare 100 . readIntCode
