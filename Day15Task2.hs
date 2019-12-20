import IntCode
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Function (on)
import Control.Monad (guard)
import Data.List (sort)
import System.Exit (exitSuccess)

infinity :: Integer
infinity = 2^64

data Position = Position {x :: Integer, y :: Integer} deriving (Eq, Ord)

data BotState = BotState {
    botPos :: Position,
    explored :: Map Position Char,
    moveDir :: Integer
    }

move :: Position -> Integer -> Position
move p 1 = Position (x p) (y p - 1)
move p 2 = Position (x p) (y p + 1)
move p 3 = Position (x p - 1) (y p)
move p 4 = Position (x p + 1) (y p)

neighbours :: Position -> [Position]
neighbours p = move p <$> [1..4]

startBot :: BotState
startBot = BotState (Position 0 0) (M.singleton (Position 0 0) '.') 1

renderScreen :: Position -> Map Position Char -> String
renderScreen bot screen = unlines $ do
    yp <- [minY..maxY]
    return $ do
        xp <- [minX..maxX]
        if (bot == Position xp yp) then
            "D"
        else
            if (xp == 0 && yp == 0) then
                "S"
            else
                case M.lookup (Position xp yp) screen of
                    Just c -> return c
                    Nothing -> " "
    where
        minY = fst $ M.findMin $ M.mapKeys y screen
        maxY = fst $ M.findMax $ M.mapKeys y screen
        minX = fst $ M.findMin $ M.mapKeys x screen
        maxX = fst $ M.findMax $ M.mapKeys x screen

{- Finds the shortest distance to something satisfying condition 1,
    subject to not going through locations satisfying condition 2,
    using function 3 to generate more locations,
    starting at the given point 4 -}
findDistance :: Eq x => (x -> Bool) -> (x -> Bool) -> (x -> [x]) -> x -> Integer
findDistance win stop more start
    | win start = 0
    | otherwise = case sort options of
        [] -> infinity
        x:_ -> x+1
    where
        options = do
            next <- more start
            guard $ not (stop next)
            return $ findDistance win (\p -> p == start || stop p) more next

{- Finds the maximum distance that can be travelled,
    subject to not going through locations satisfying condition 1,
    using function 2 to generate more locations,
    starting at the given poin 3 -}
maxDistance :: Eq x => (x -> Bool) -> (x -> [x]) -> x -> Integer
maxDistance stop more start = maximum (0:options) where
    options = do
        next <- more start
        guard $ not (stop next)
        return $ succ $ maxDistance (\p -> p == start || stop p) more next

getOptions :: Position -> Map Position Char -> [(Integer, Integer)]
getOptions position screen = do
    dir <- [1..4]
    guard $ not $ isWall $ move position dir
    return (findDistance isEmpty isWall neighbours (move position dir), dir)
    where
        isWall p = p == position || case M.lookup p screen of
            Just '#' -> True
            _ -> False
        isEmpty p = case M.lookup p screen of
            Nothing -> True
            _ -> False

getMove :: Position -> Map Position Char -> (Integer, Integer)
getMove position screen = head $ sort $ getOptions position screen

updateState :: Integer -> BotState -> IO BotState
updateState n state
    | moveDist >= infinity = do
        putStrLn $ renderScreen (Position infinity infinity) newScreen
        putStrLn "Furthest point from oxygen is:"
        print $ maxDistance ((== Just '#') . flip M.lookup newScreen) neighbours oxygenPos
        exitSuccess
    | otherwise = return $ BotState newPos newScreen newDir
    where
        movedPos = move (botPos state) (moveDir state)
        newPos = case n of
            0 -> botPos state
            _ -> movedPos
        newBlock = case n of
            0 -> '#'
            1 -> '.'
            2 -> 'O'
            _ -> error "Invalid value from IntCode"
        newScreen = M.insert movedPos newBlock (explored state)
        (moveDist, newDir) = getMove newPos newScreen
        Just (oxygenPos, _) = M.lookupMin $ M.filter (== 'O') newScreen

main :: IO ()
main = do
    intcode <- readIntCode <$> readFile "Day15Input"
    runSimpleRead startBot updateState ((,) <$> id <*> moveDir) intcode
    print "IntCode terminated before goal was found"
