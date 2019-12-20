import IntCode
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (unfoldr, intersperse)

data Position = Position { x :: Int, y :: Int } deriving (Eq, Ord)

data MapState = MapState {
    screen :: Map Position Char,
    nextPos :: Position
}

data Direction = U | R | D | L deriving Enum

clockwise :: Direction -> Direction
clockwise L = U
clockwise x = succ x

anticlock :: Direction -> Direction
anticlock U = L
anticlock x = pred x

move :: Direction -> Position -> Position
move U p = Position (x p) (y p - 1)
move R p = Position (x p + 1) (y p)
move D p = Position (x p) (y p + 1)
move L p = Position (x p - 1) (y p)

fromInt :: Integer -> Char
fromInt = toEnum . fromInteger

toInt :: String -> [Integer]
toInt = fmap $ toInteger . fromEnum

nextCol :: Position -> Position
nextCol p = Position (x p + 1) (y p)

nextRow :: Position -> Position
nextRow p = Position 0 (y p + 1)

updateMap :: Integer -> MapState -> MapState
updateMap 10 s = MapState
    (screen s)
    (nextRow $ nextPos s)
updateMap c s = MapState
    (M.insert (nextPos s) (fromInt c) $ screen s)
    (nextCol $ nextPos s)

startMap :: MapState
startMap = MapState M.empty (Position 0 0)

renderMap :: Map Position Char -> String
renderMap m = unlines $ do
    yp <- [minY..maxY]
    return $ do
        xp <- [minX..maxX]
        case M.lookup (Position xp yp) m of
            Just c -> return c
            Nothing -> " "
    where
        minY = fst $ M.findMin $ M.mapKeys y m
        maxY = fst $ M.findMax $ M.mapKeys y m
        minX = fst $ M.findMin $ M.mapKeys x m
        maxX = fst $ M.findMax $ M.mapKeys x m

data PathState = PathState {
    scaffold :: Set Position,
    unvisited :: Set Position,
    curPos :: Position,
    curDir :: Direction
}

data Instruction = RL | RR | M Int

render :: Instruction -> String
render RL = "L"
render RR = "R"
render (M x) = show x

renderInstructs :: [Instruction] -> String
renderInstructs = concat . intersperse "," . fmap render

startPath :: Map Position Char -> PathState
startPath m = PathState scaff (startPos `S.delete` scaff) startPos startDir where
    scaff = M.keysSet $ M.filter (/= '.') m
    (startPos, dirChar) = M.findMin $ M.filter (`elem` "<>v^") m
    startDir = case dirChar of
        '^' -> U
        '>' -> R
        'v' -> D
        '<' -> L

next :: PathState -> Maybe (Instruction, PathState)
next s
    | S.null (unvisited s) = Nothing
    | move (curDir s) (curPos s) `S.member` scaffold s =
        Just (M 1, PathState
            (scaffold s)
            (move (curDir s) (curPos s) `S.delete` unvisited s)
            (move (curDir s) (curPos s))
            (curDir s)
        )
    | move (clockwise $ curDir s) (curPos s) `S.member` scaffold s =
        Just (RR, PathState
            (scaffold s)
            (unvisited s)
            (curPos s)
            (clockwise $ curDir s)
        )
    | move (anticlock $ curDir s) (curPos s) `S.member` scaffold s =
        Just (RL, PathState
            (scaffold s)
            (unvisited s)
            (curPos s)
            (anticlock $ curDir s)
        )
    | otherwise = error "No way to continue"

getPath :: Map Position Char -> [Instruction]
getPath = unfoldr next . startPath

simplifyPath :: [Instruction] -> [Instruction]
simplifyPath (RL:RL:RL:is) = simplifyPath (RR:is)
simplifyPath (RR:RR:RR:is) = simplifyPath (RL:is)
simplifyPath (RL:RR:is) = simplifyPath is
simplifyPath (RR:RL:is) = simplifyPath is
simplifyPath (RL:is) = RL:simplifyPath is
simplifyPath (RR:is) = RR:simplifyPath is
simplifyPath (M x: M y:is) = simplifyPath (M (x+y):is)
simplifyPath (M x:is) = M x:simplifyPath is
simplifyPath [] = []

finalCommand :: String
finalCommand = unlines $ [
    "A,B,A,C,B,A,B,C,C,B",
    "L,12,L,12,R,4",
    "R,10,R,6,R,4,R,4",
    "R,6,L,12,L,12",
    "n"
    ]

type FinalState = ([Integer], Integer)

startFinalState :: FinalState
startFinalState = (toInt finalCommand, 0)

updateFinalState :: Integer -> FinalState -> FinalState
updateFinalState new (outs, old) = (outs, new)

readFinalState :: FinalState -> (FinalState, Integer)
readFinalState (out:outs, i) = ((outs, i), out)

-- Final solution
main :: IO ()
main = do
    intcode <- (2:) . tail . readIntCode <$> readFile "Day17Input"
    print $ snd $ runSimple startFinalState updateFinalState readFinalState intcode

-- Map reading
-- main :: IO ()
-- main = do
--     intcode <- readIntCode <$> readFile "Day17Input"
--     let finalState = runSimple startMap updateMap undefined intcode in do
--         -- Just print the map
--         -- putStrLn $ renderMap $ screen finalState
--         -- Print out the route
--         putStrLn $ renderInstructs $ simplifyPath $ getPath $ screen finalState
--         -- Route is
--         -- A = L,12,L,12,R,4,
--         -- B = R,10,R,6,R,4,R,4,
--         -- A = L,12,L,12,R,4,
--         -- C = R,6,L,12,L,12,
--         -- B = R,10,R,6,R,4,R,4,
--         -- A = L,12,L,12,R,4,
--         -- B = R,10,R,6,R,4,R,4,
--         -- C = R,6,L,12,L,12,
--         -- C = R,6,L,12,L,12,
--         -- B = R,10,R,6,R,4,R,4
