import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)
import qualified Data.Set as S
import Data.Set (Set)

infixl 9 !
(!) :: Map Integer Integer -> Integer -> Integer
m ! k = M.findWithDefault 0 k m

data IntState = IntState {
    values :: Map Integer Integer,
    pointer :: Integer,
    bot :: BotState,
    relbase :: Integer
    }

run :: IntState -> Either BotState IntState
run state = case opcode of
    99 -> Left $ bot state
    1 -> Right $ IntState
        (M.insert (getAddr 3) (lookupRel 1 + lookupRel 2) $ values state)
        (pointer state + 4)
        (bot state)
        (relbase state)
    2 -> Right $ IntState
        (M.insert (getAddr 3) (lookupRel 1 * lookupRel 2) $ values state)
        (pointer state + 4)
        (bot state)
        (relbase state)
    3 -> Right $ IntState
        (M.insert (getAddr 1) (readColour $ bot state) $ values state)
        (pointer state + 2)
        (bot state)
        (relbase state)
    4 -> Right $ IntState
        (values state)
        (pointer state + 2)
        (updateBot (bot state) (lookupRel 1))
        (relbase state)
    5 -> Right $ IntState
        (values state)
        (if lookupRel 1 /= 0 then lookupRel 2 else pointer state + 3)
        (bot state)
        (relbase state)
    6 -> Right $ IntState
        (values state)
        (if lookupRel 1 == 0 then lookupRel 2 else pointer state + 3)
        (bot state)
        (relbase state)
    7 -> Right $ IntState
        (M.insert (getAddr 3) (if lookupRel 1 < lookupRel 2 then 1 else 0) $ values state)
        (pointer state + 4)
        (bot state)
        (relbase state)
    8 -> Right $ IntState
        (M.insert (getAddr 3) (if lookupRel 1 == lookupRel 2 then 1 else 0) $ values state)
        (pointer state + 4)
        (bot state)
        (relbase state)
    9 -> Right $ IntState
        (values state)
        (pointer state + 2)
        (bot state)
        (relbase state + lookupRel 1)
    _ -> error "Undefined opcode"
    where
        getRel i = values state ! (pointer state + i)
        opcode = getRel 0 `mod` 100
        mode i = (getRel 0 `div` (10 ^ (i + 1))) `mod` 10
        getAddr i
            | mode i == 0 = getRel i
            | mode i == 1 = error "Invalid mode to read/write from"
            | mode i == 2 = getRel i + relbase state
        lookupRel i
            | mode i == 0 = values state ! getAddr i
            | mode i == 1 = getRel i
            | mode i == 2 = values state ! getAddr i

runforever :: IntState -> BotState
runforever state = case run state of
    Left result -> result
    Right newstate -> runforever newstate

makeState :: [Integer] -> IntState
makeState vals = IntState (M.fromDistinctAscList $ zip [0..] vals) 0 startBot 0

data Position = Position {x :: Integer, y :: Integer} deriving (Eq, Ord)

data Direction = U | R | D | L deriving (Eq, Ord, Enum)

data BotState = BotState {
    whites :: Set Position,
    pos :: Position,
    dir :: Direction,
    paintNext :: Bool
    }

rotate :: Direction -> Integer -> Direction
rotate U 0 = L
rotate x 0 = pred x
rotate L 1 = U
rotate x 1 = succ x
rotate _ _ = error "Invalid rotation direction"

move :: Position -> Direction -> Position
move p U = Position (x p) (y p - 1)
move p D = Position (x p) (y p + 1)
move p R = Position (x p + 1) (y p)
move p L = Position (x p - 1) (y p)

paint :: Set Position -> Position -> Integer -> Set Position
paint s p 0 = S.delete p s
paint s p 1 = S.insert p s
paint _ _ _ = error "Invalid paint colour"

updateBot :: BotState -> Integer -> BotState
updateBot state input
    | paintNext state = BotState
        (paint (whites state) (pos state) input)
        (pos state)
        (dir state)
        False
    | otherwise = BotState
        (whites state)
        (move (pos state) (rotate (dir state) input))
        (rotate (dir state) input)
        True

readColour :: BotState -> Integer
readColour state = if S.member (pos state) (whites state) then 1 else 0

startBot :: BotState
startBot = BotState (S.singleton $ Position 0 0) (Position 0 0) U True

printPaint :: Set Position -> String
printPaint whites = unlines $ do
    y <- [minY..maxY]
    return $ do
        x <- [minX..maxX]
        if S.member (Position x y) whites then "#" else " "
    where
        minY = S.findMin $ S.map y whites
        maxY = S.findMax $ S.map y whites
        minX = S.findMin $ S.map x whites
        maxX = S.findMax $ S.map x whites

main :: IO ()
main = interact $ printPaint . whites . runforever . makeState . fmap read . words . fmap (\x -> if x == ',' then ' ' else x)
