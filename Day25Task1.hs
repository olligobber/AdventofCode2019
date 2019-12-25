import IntCode

readInput :: [Integer] -> IO ([Integer], Integer)
readInput (x:xs) = return (xs, x)
readInput [] = do
    l <- fmap (toInteger . fromEnum) <$> getLine
    case l of
        (x:xs) -> return (xs ++ [10], x)
        [] -> return ([], 10)

writeOutput :: Integer -> [Integer] -> IO [Integer]
writeOutput c l = do
    putChar $ toEnum $ fromInteger c
    return l

main :: IO ()
main = do
    intcode <- readIntCode <$> readFile "Day25Input"
    () <$ run [] writeOutput readInput intcode
