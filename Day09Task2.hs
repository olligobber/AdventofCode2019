import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)
import Control.Monad (guard)
import Data.List (permutations)

infixl 9 !
(!) :: Map Integer Integer -> Integer -> Integer
m ! k = M.findWithDefault 0 k m

data State = State {
    values :: Map Integer Integer,
    pointer :: Integer,
    inputs :: [Integer],
    outputs :: [Integer],
    relbase :: Integer
    }

run :: State -> Either [Integer] State
run state = case opcode of
    99 -> Left $ reverse $ outputs state
    1 -> Right $ State
        (M.insert (getAddr 3) (lookupRel 1 + lookupRel 2) $ values state)
        (pointer state + 4)
        (inputs state)
        (outputs state)
        (relbase state)
    2 -> Right $ State
        (M.insert (getAddr 3) (lookupRel 1 * lookupRel 2) $ values state)
        (pointer state + 4)
        (inputs state)
        (outputs state)
        (relbase state)
    3 -> Right $ State
        (M.insert (getAddr 1) (head $ inputs state) $ values state)
        (pointer state + 2)
        (tail $ inputs state)
        (outputs state)
        (relbase state)
    4 -> Right $ State
        (values state)
        (pointer state + 2)
        (inputs state)
        (lookupRel 1 : outputs state)
        (relbase state)
    5 -> Right $ State
        (values state)
        (if lookupRel 1 /= 0 then lookupRel 2 else pointer state + 3)
        (inputs state)
        (outputs state)
        (relbase state)
    6 -> Right $ State
        (values state)
        (if lookupRel 1 == 0 then lookupRel 2 else pointer state + 3)
        (inputs state)
        (outputs state)
        (relbase state)
    7 -> Right $ State
        (M.insert (getAddr 3) (if lookupRel 1 < lookupRel 2 then 1 else 0) $ values state)
        (pointer state + 4)
        (inputs state)
        (outputs state)
        (relbase state)
    8 -> Right $ State
        (M.insert (getAddr 3) (if lookupRel 1 == lookupRel 2 then 1 else 0) $ values state)
        (pointer state + 4)
        (inputs state)
        (outputs state)
        (relbase state)
    9 -> Right $ State
        (values state)
        (pointer state + 2)
        (inputs state)
        (outputs state)
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

runforever :: State -> [Integer]
runforever state = case run state of
    Left result -> result
    Right newstate -> runforever newstate

makeState :: [Integer] -> [Integer] -> State
makeState vals inputs = State (M.fromDistinctAscList $ zip [0..] vals) 0 inputs [] 0

main :: IO ()
main = interact $ show . runforever . flip makeState [2] . fmap read . words . fmap (\x -> if x == ',' then ' ' else x)
