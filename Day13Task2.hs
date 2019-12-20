import IntCode
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data InputState = NoInput | GotX Integer | GotXY Integer Integer

data ArcadeState = ArcadeState {
    screen :: Map (Integer, Integer) Integer,
    score :: Integer,
    ballX :: Integer,
    paddleX :: Integer
}

type WholeState = (InputState, ArcadeState)

updateInput :: Integer -> InputState -> (InputState, Maybe (Integer, Integer, Integer))
updateInput i NoInput = (GotX i, Nothing)
updateInput i (GotX x) = (GotXY x i, Nothing)
updateInput i (GotXY x y) = (NoInput, Just (x,y,i))

updateArcade :: (Integer, Integer, Integer) -> ArcadeState -> ArcadeState
updateArcade (-1,0,s) a = ArcadeState (screen a) s (ballX a) (paddleX a)
updateArcade (x,y,4) a = ArcadeState (M.insert (x,y) 4 $ screen a) (score a) x (paddleX a)
updateArcade (x,y,3) a = ArcadeState (M.insert (x,y) 3 $ screen a) (score a) (ballX a) x
updateArcade (x,y,t) a = ArcadeState (M.insert (x,y) t $ screen a) (score a) (ballX a) (paddleX a)

updateState :: Integer -> WholeState -> WholeState
updateState i (ins, ars) = case updateInput i ins of
    (newins, Nothing) -> (newins, ars)
    (newins, Just j) -> (newins, updateArcade j ars)

startInput :: InputState
startInput = NoInput

startArcade :: ArcadeState
startArcade = ArcadeState M.empty 0 0 0

startState :: WholeState
startState = (startInput, startArcade)

showFinish :: WholeState -> Integer
showFinish (_, a) = score a

renderBlock :: Integer -> Char
renderBlock 0 = ' '
renderBlock 1 = '#'
renderBlock 2 = '$'
renderBlock 3 = '~'
renderBlock 4 = 'o'
renderBlock _ = '?'

renderScreen :: Map (Integer, Integer) Integer -> String
renderScreen s = unlines $ do
    y <- [minY..maxY]
    return $ do
        x <- [minX..maxX]
        case M.lookup (x,y) s of
            Just c -> return $ renderBlock c
            Nothing -> "?"
    where
        minY = fst $ M.findMin $ M.mapKeys snd s
        maxY = fst $ M.findMax $ M.mapKeys snd s
        minX = fst $ M.findMin $ M.mapKeys fst s
        maxX = fst $ M.findMax $ M.mapKeys fst s

getPaddle :: IO Integer
getPaddle = do
    putStrLn "Type L, W, R to move Left, Wait, or Right"
    i <- getLine
    case i of
        "L" -> return (-1)
        "W" -> return 0
        "R" -> return 1
        _ -> getPaddle

prompt :: WholeState -> IO Integer
prompt (_, a) = do
    putStrLn $ renderScreen $ screen a
    putStrLn $ "Score: " ++ show (score a)
    getPaddle

autoMove :: WholeState -> Integer
autoMove (_,a) = case ballX a `compare` paddleX a of
    LT -> (-1)
    EQ -> 0
    GT -> 1

autoMoveRender :: WholeState -> IO (WholeState, Integer)
autoMoveRender w@(_,a) = do
    putStrLn $ renderScreen $ screen a
    putStrLn $ "Score: " ++ show (score a)
    putStrLn ""
    return (w, autoMove w)

-- Automove with rendering
main :: IO ()
main = do
    intcode <- readIntCode <$> readFile "Day13Input"
    endState <- runSimpleWrite startState updateState autoMoveRender $ (2:) $ tail intcode
    putStrLn "Game Over"
    print $ showFinish endState

-- Manual gameplay
-- main :: IO ()
-- main = do
--     intcode <- readIntCode <$> readFile "Day13Input"
--     endState <- runSimpleWrite startState updateState prompt $ (2:) $ tail intcode
--     putStrLn "Game Over"
--     print $ showFinish endState
