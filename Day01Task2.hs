fuel :: Int -> Int
fuel = (+ (-2)) . (`div` 3)

actualfuel :: Int -> Int
actualfuel n
    | fuel n <= 0   = 0
    | otherwise     = fuel n + actualfuel (fuel n)

main :: IO ()
main = interact $ show . sum . fmap actualfuel . fmap read . lines
