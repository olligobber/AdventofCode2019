import IntCode
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List (uncons)

-- data ReadAddr = A | B | C | D | E | F | G | H | I deriving (Eq, Ord, Enum, Show)
data ReadAddr = A | B | C | D deriving (Eq, Ord, Enum, Show)

data WriteAddr = J | T deriving (Eq, Ord, Show)

data Addr = R ReadAddr | W WriteAddr deriving (Eq, Ord)

allAddr :: [Addr]
allAddr = r ++ w where
    -- r = R <$> [A .. I]
    r = R <$> [A .. D]
    w = W <$> [J,T]

renderAddr :: Addr -> String
renderAddr (R x) = show x
renderAddr (W x) = show x

data Op = AND | OR | NOT deriving (Eq, Enum, Show)

data SpringCode = SpringCode {op :: Op, from :: Addr, to :: WriteAddr} deriving Eq

allLines :: [SpringCode]
allLines = SpringCode <$> [AND .. NOT] <*> allAddr <*> [J,T]

renderLine :: SpringCode -> String
renderLine s = unwords [show $ op s, renderAddr $ from s, show $ to s]

-- Determines if it is worth doing one line after the other
worthwhile :: SpringCode -> SpringCode -> Bool
worthwhile a b = all not [
    a == b, -- repeating the same operation
    to a == to b && from b /= W (to b) && op b == NOT, -- overwriting the value you just wrote
    W (to a) == from a && (op a == AND || op a == OR), -- idempotence of and/or
    W (to b) == from b && (op b == AND || op b == OR), -- idempotence of and/or
    to a == to b && from a == from b && op a /= NOT -- with same addresses is pointless except if first is not
    ]

type Vars = Map Addr Bool

type Code = [SpringCode]

renderCode :: Code -> String
-- renderCode c = unlines $ fmap renderLine c ++ ["RUN"]
renderCode c = unlines $ fmap renderLine c ++ ["WALK"]

doesHead :: Bool -> (a -> Bool) -> [a] -> Bool
doesHead b _ [] = b
doesHead _ p (x:_) = p x

allProgramsLength :: Int -> [Code]
allProgramsLength n
    | n < 0 = []
    | otherwise = []: do
        line <- allLines
        rest <- filter (doesHead True $ worthwhile line) $ allProgramsLength (n-1)
        return $ line:rest

data IOController = IOController {
    input :: String,
    output :: Integer
}

makeIOController :: Code -> IOController
makeIOController = flip IOController 0 . renderCode

readIOController :: IOController -> (IOController, Integer)
readIOController i = (IOController (tail $ input i) (output i), toInteger $ fromEnum $ head $ input i)

writeIOController :: Integer -> IOController -> IOController
writeIOController n i = IOController (input i) n

doAllPrograms :: [Code] -> [Integer] -> Integer
doAllPrograms [] _ = error "All programs used"
doAllPrograms (thisp:restp) intcode
    | thisresult > 127 = thisresult
    | otherwise = doAllPrograms restp intcode
    where
        thisresult = output $
            runSimple (makeIOController thisp) writeIOController readIOController intcode

main :: IO ()
main = interact $ (++"\n") . show . doAllPrograms (allProgramsLength 15) . readIntCode
