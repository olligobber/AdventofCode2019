import IntCode

neg :: Char -> String
neg x = onop x x "NOT"

onop :: Char -> Char -> String -> String
onop a o op = unwords [op, [a], [o]]

-- Given two bits to read from and a bit to write to, makes most possible programs to combine C and D onto that bit
twoBitPrograms :: Char -> Char -> Char -> [String]
twoBitPrograms c d x = [
    onop x x "OR", -- always false
    neg x, -- always true
    onop c x "OR", -- always C
    onop c x "NOT", -- always not C
    onop d x "OR", -- always D
    onop d x "NOT", -- always not D
    init $ unlines [onop c x "OR", onop d x "AND"], -- C and D
    init $ unlines [onop c x "OR", onop d x "AND", neg x], -- C nand D
    init $ unlines [onop c x "OR", onop d x "OR"], -- C or D
    init $ unlines [onop c x "OR", onop d x "OR", neg x], -- C nor D
    init $ unlines [onop c x "NOT", onop d x "OR"], -- C implies D
    init $ unlines [onop c x "NOT", onop d x "AND"], -- D nimplies C
    init $ unlines [onop d x "NOT", onop c x "OR"], -- D implies C
    init $ unlines [onop d x "NOT", onop c x "AND"] --  C nimplies D
    -- C equals D
    -- C nequals D
    ]

threeBitPrograms :: [String]
threeBitPrograms = do
    [b,c,d] <- ["BCD", "CBD", "DBC"]
    whenBTrue <- twoBitPrograms c d 'T'
    whenBFalse <- twoBitPrograms c d 'J'
    return $ init $ unlines [
        whenBFalse,
        onop b 'T' "NOT",
        onop 'T' 'J' "AND",
        onop b 'T' "AND",
        whenBTrue,
        onop b 'T' "AND",
        onop 'T' 'J' "OR"
        ]

allPrograms :: [String]
allPrograms = do
    threebit <- threeBitPrograms
    return $ unlines [
        threebit,
        onop 'A' 'T' "NOT",
        onop 'T' 'J' "OR",
        "WALK"
        ]

data IOController = IOController {
    input :: String,
    output :: Integer
}

makeIOController :: String -> IOController
makeIOController = flip IOController 0

readIOController :: IOController -> (IOController, Integer)
readIOController i = (IOController (tail $ input i) (output i), toInteger $ fromEnum $ head $ input i)

writeIOController :: Integer -> IOController -> IOController
writeIOController n i = IOController (input i) n

doAllPrograms :: [Integer] -> Integer
doAllPrograms intcode = maximum $ do
    program <- makeIOController <$> allPrograms
    return $ output $
        runSimple program writeIOController readIOController intcode

main :: IO ()
main = interact $ (++"\n") . show . doAllPrograms . readIntCode
