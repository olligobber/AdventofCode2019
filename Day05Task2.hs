import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map, (!))
import Control.Monad (guard)

data State = State {
    values :: Map Int Int,
    pointer :: Int,
    inputs :: [Int],
    outputs :: [Int]
    }

run :: State -> Either [Int] State
run state = case opcode of
    99 -> Left $ outputs state
    1 -> Right $ State
        (M.insert (getRel 3) (lookupRel 1 + lookupRel 2) $ values state)
        (pointer state + 4)
        (inputs state)
        (outputs state)
    2 -> Right $ State
        (M.insert (getRel 3) (lookupRel 1 * lookupRel 2) $ values state)
        (pointer state + 4)
        (inputs state)
        (outputs state)
    3 -> Right $ State
        (M.insert (getRel 1) (head $ inputs state) $ values state)
        (pointer state + 2)
        (tail $ inputs state)
        (outputs state)
    4 -> Right $ State
        (values state)
        (pointer state + 2)
        (inputs state)
        (lookupRel 1 : outputs state)
    5 -> Right $ State
        (values state)
        (if lookupRel 1 /= 0 then lookupRel 2 else pointer state + 3)
        (inputs state)
        (outputs state)
    6 -> Right $ State
        (values state)
        (if lookupRel 1 == 0 then lookupRel 2 else pointer state + 3)
        (inputs state)
        (outputs state)
    7 -> Right $ State
        (M.insert (getRel 3) (if lookupRel 1 < lookupRel 2 then 1 else 0) $ values state)
        (pointer state + 4)
        (inputs state)
        (outputs state)
    8 -> Right $ State
        (M.insert (getRel 3) (if lookupRel 1 == lookupRel 2 then 1 else 0) $ values state)
        (pointer state + 4)
        (inputs state)
        (outputs state)
    _ -> error "Undefined opcode"
    where
        getRel i = values state ! (pointer state + i)
        opcode = getRel 0 `mod` 100
        mode i = (getRel 0 `div` (10 ^ (i + 1))) `mod` 10
        lookupRel i
            | mode i == 0 = values state ! (getRel i)
            | mode i == 1 = getRel i

runforever :: State -> [Int]
runforever state = case run state of
    Left result -> result
    Right newstate -> runforever newstate

makeState :: [Int] -> [Int] -> State
makeState vals inputs = State (M.fromDistinctAscList $ zip [0..] vals) 0 inputs []

main :: IO ()
main = interact $ show . runforever . flip makeState [5] . fmap read . words . fmap (\x -> if x == ',' then ' ' else x)
