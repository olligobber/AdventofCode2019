import IntCode
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

data Position = Position { x :: Int, y :: Int } deriving (Eq, Ord)

data MapState = MapState {
    isScaffold :: Map Position Bool,
    nextPos :: Position
}

neighbours :: Position -> [Position]
neighbours p = do
    (dx,dy) <- [(1,0),(-1,0),(0,1),(0,-1)]
    return $ Position (x p + dx) (y p + dy)

nextCol :: Position -> Position
nextCol p = Position (x p + 1) (y p)

nextRow :: Position -> Position
nextRow p = Position 0 (y p + 1)

updateMap :: Integer -> MapState -> MapState
updateMap 35 s = MapState
    (M.insert (nextPos s) True $ isScaffold s)
    (nextCol $ nextPos s)
updateMap 46 s = MapState
    (M.insert (nextPos s) False $ isScaffold s)
    (nextCol $ nextPos s)
updateMap 10 s = MapState
    (isScaffold s)
    (nextRow $ nextPos s)
updateMap 94 s = updateMap 35 s -- Treat bot as scaffold
updateMap 118 s = updateMap 35 s -- Treat bot as scaffold
updateMap 60 s = updateMap 35 s -- Treat bot as scaffold
updateMap 62 s = updateMap 35 s -- Treat bot as scaffold
updateMap x _ = error $ "Unknown camera readout: " ++ show x

startMap :: MapState
startMap = MapState M.empty (Position 0 0)

getThisAlignment :: Map Position Bool -> Position -> Bool -> Int
getThisAlignment _ _ False = 0
getThisAlignment m p True
    | all ((== Just True) . flip M.lookup m) (neighbours p) = x p * y p
    | otherwise = 0

getAlignment :: Map Position Bool -> Int
getAlignment m = sum $ M.mapWithKey (getThisAlignment m) m

renderMap :: Map Position Bool -> String
renderMap m = unlines $ do
    yp <- [minY..maxY]
    return $ do
        xp <- [minX..maxX]
        case M.lookup (Position xp yp) m of
            Just True -> "#"
            Just False -> "."
            Nothing -> " "
    where
        minY = fst $ M.findMin $ M.mapKeys y m
        maxY = fst $ M.findMax $ M.mapKeys y m
        minX = fst $ M.findMin $ M.mapKeys x m
        maxX = fst $ M.findMax $ M.mapKeys x m

main :: IO ()
main = do
    intcode <- readIntCode <$> readFile "Day17Input"
    let finalState = runSimple startMap updateMap undefined intcode in do
        -- putStrLn $ renderMap $ isScaffold finalState
        print $ getAlignment $ isScaffold finalState
