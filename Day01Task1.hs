fuel :: Int -> Int
fuel = (+ (-2)) . (`div` 3)

main :: IO ()
main = interact $ show . sum . fmap fuel . fmap read . lines
