import IntCode

scanCoord :: [Integer] -> Integer -> Integer -> Integer
scanCoord intcode x y = result where
    Left result = runSimpleRead [x,y] (const . Left) (\(a:as) -> (as,a)) intcode

scanCoords :: [Integer] -> Integer
scanCoords intcode = sum $ scanCoord intcode <$> [0..49] <*> [0..49]

main :: IO ()
main = scanCoords . readIntCode <$> readFile "Day19Input" >>= print
