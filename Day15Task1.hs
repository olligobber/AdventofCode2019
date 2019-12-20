import IntCode
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Function (on)
import Control.Monad (guard)
import Data.List (sort, (!!))
import System.Exit (exitSuccess)
import System.Console.ANSI (clearScreen)

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
            case M.lookup (Position xp yp) screen of
                Just c -> return c
                Nothing -> " "
    where
        minY = fst $ M.findMin $ M.mapKeys y screen
        maxY = fst $ M.findMax $ M.mapKeys y screen
        minX = fst $ M.findMin $ M.mapKeys x screen
        maxX = fst $ M.findMax $ M.mapKeys x screen

prompt :: Position -> Map Position Char -> IO Integer
prompt p m = do
    putStrLn "Type w,a,s,d to move, c to get automove info, or press anything else to automove"
    x <- getLine
    case x of
        'w':_ -> return 1
        's':_ -> return 2
        'a':_ -> return 3
        'd':_ -> return 4
        'c':_ -> do
            putStrLn $ unlines $ do
                (distance, dir) <- getOptions p m
                return $ "Option " ++ ["xwasd" !! fromInteger dir] ++ " has distance " ++ show distance
            prompt p m
        _ -> return $ getMove p m

updateStateIO :: Integer -> BotState -> IO BotState
updateStateIO 2 state = let
    finalPos = move (botPos state) (moveDir state)
    finalScreen = M.insert finalPos '.' (explored state)
    isBad p = case M.lookup p finalScreen of
        Just '#' -> True
        Nothing -> True
        _ -> False
    in do
        putStrLn $ renderScreen finalPos finalScreen
        putStrLn "Maze complete, minimum distance is:"
        print $ findDistance (== Position 0 0) isBad neighbours finalPos
        exitSuccess
updateStateIO 1 state = let
    newPos = move (botPos state) (moveDir state)
    newScreen = M.insert newPos '.' (explored state)
    in do
        putStrLn $ renderScreen newPos newScreen
        BotState newPos newScreen <$> prompt newPos newScreen
updateStateIO 0 state = let
    wallPos = move (botPos state) (moveDir state)
    newScreen = M.insert wallPos '#' (explored state)
    in do
        putStrLn $ renderScreen (botPos state) newScreen
        BotState (botPos state) newScreen <$> prompt (botPos state) newScreen
updateStateIO _ _ = error "Invalid value from IntCode"

{- Finds the shortest distance to something satisfying condition 1,
    subject to not going through locations satisfying condition 2,
    using function 3 to generate more locations,
    starting at the given point 4 -}
findDistance :: Eq x => (x -> Bool) -> (x -> Bool) -> (x -> [x]) -> x -> Integer
findDistance win stop more start
    | win start = 0
    | otherwise = case sort options of
        [] -> 2^127
        x:_ -> x+1
    where
        options = do
            next <- more start
            guard $ not (stop next)
            return $ findDistance win (\p -> p == start || stop p) more next

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

getMove :: Position -> Map Position Char -> Integer
getMove position screen = snd $ head $ sort $ getOptions position screen

updateState :: Integer -> BotState -> IO BotState
updateState 2 state = let
    finalPos = move (botPos state) (moveDir state)
    finalMap = M.insert finalPos '.' (explored state)
    isBad p = case M.lookup p finalMap of
        Just '#' -> True
        Nothing -> True
        _ -> False
    in do
        clearScreen
        putStrLn "Maze complete, minimum distance is:"
        print $ findDistance (== Position 0 0) isBad neighbours finalPos
        exitSuccess
updateState 1 state = let
    newPos = move (botPos state) (moveDir state)
    newScreen = M.insert newPos '.' (explored state)
    newDir = getMove newPos newScreen
    in do
        clearScreen
        putStrLn $ renderScreen newPos newScreen
        return $ BotState newPos newScreen newDir
updateState 0 state = let
    wallPos = move (botPos state) (moveDir state)
    newScreen = M.insert wallPos '#' (explored state)
    newDir = getMove (botPos state) newScreen
    in do
        clearScreen
        putStrLn $ renderScreen (botPos state) newScreen
        return $ BotState (botPos state) newScreen newDir
updateState _ _ = error "Invalid value from IntCode"

main :: IO ()
main = do
    intcode <- readIntCode <$> readFile "Day15Input"
    runSimpleRead startBot updateState ((,) <$> id <*> moveDir) intcode
    -- runSimpleRead startBot updateStateIO ((,) <$> id <*> moveDir) intcode
    print "IntCode terminated before goal was found"
