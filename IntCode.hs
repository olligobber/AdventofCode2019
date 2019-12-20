module IntCode (
    run,
    runSimple,
    runSimpleRead,
    runSimpleWrite,
    readIntCode
    ) where

import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)
import Control.Monad.Identity (Identity(..))

-- Run int code using monadic io
run :: Monad m => x -> (Integer -> x -> m x) -> (x -> m (x, Integer)) -> [Integer] -> m x
run start w r vals = runforever $ makestate vals start w r

-- Run int code using simple io
runSimple :: x -> (Integer -> x -> x) -> (x -> (x, Integer)) -> [Integer] -> x
runSimple start w r = runIdentity . run start (\i s -> Identity $ w i s) (Identity . r)

-- Run int code with monadic read, simple write
runSimpleWrite :: Monad m => x -> (Integer -> x -> x) -> (x -> m (x, Integer)) -> [Integer] -> m x
runSimpleWrite start w r = run start (\i s -> return $ w i s) r

-- Run int code with simple read, monadic write
runSimpleRead :: Monad m => x -> (Integer -> x -> m x) -> (x -> (x, Integer)) -> [Integer] -> m x
runSimpleRead start w r = run start w (return . r)

-- Convert comma seperated numbers to a list of integers
readIntCode :: String -> [Integer]
readIntCode = fmap read . words . fmap (\x -> if x == ',' then ' ' else x)

-- Read map but return 0 if value does not exist
infixl 9 !
(!) :: Map Integer Integer -> Integer -> Integer
m ! k = M.findWithDefault 0 k m

data IntState m x = IntState {
    values :: Map Integer Integer, -- Current list of values in computation
    pointer :: Integer, -- Current location of pointer
    relbase :: Integer, -- Basepoint for relative mode
    iostate :: x, -- Internal state for read/write
    writeio :: Integer -> x -> m x, -- Command to write to internal state
    readio :: x -> m (x, Integer) -- Command to read from internal state
}

{- Do one step of computation
    nextState takes in 4 functions which are used to mutate the
    values, pointer, relbase, and iostate respectively -}
step :: Monad m => IntState m x -> m (Either x (IntState m x))
step state = case opcode of
    99 -> return $ Left $ iostate state
    1 -> nextState
        (M.insert (getAddr 3) (lookupRel 1 + lookupRel 2))
        (+4)
        id
        id
    2 -> nextState
        (M.insert (getAddr 3) (lookupRel 1 * lookupRel 2))
        (+4)
        id
        id
    3 -> readio state (iostate state) >>= \(s,r) -> nextState
        (M.insert (getAddr 1) r)
        (+2)
        id
        (const s)
    4 -> writeio state (lookupRel 1) (iostate state) >>= \s -> nextState
        id
        (+2)
        id
        (const s)
    5 -> nextState
        id
        (if lookupRel 1 /= 0 then const $ lookupRel 2 else (+3))
        id
        id
    6 -> nextState
        id
        (if lookupRel 1 == 0 then const $ lookupRel 2 else (+3))
        id
        id
    7 -> nextState
        (M.insert (getAddr 3) (if lookupRel 1 < lookupRel 2 then 1 else 0))
        (+4)
        id
        id
    8 -> nextState
        (M.insert (getAddr 3) (if lookupRel 1 == lookupRel 2 then 1 else 0))
        (+4)
        id
        id
    9 -> nextState
        id
        (+2)
        (+lookupRel 1)
        id
    x -> error $ "Undefined opcode " ++ show x ++ " at index " ++ show (pointer state)
    where
        nextState cv cp cr ci = return $ Right $ IntState
            (cv $ values state)
            (cp $ pointer state)
            (cr $ relbase state)
            (ci $ iostate state)
            (writeio state)
            (readio state)
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

-- Continue to do steps until either the monadic bind does nothing, or opcode 99 is executed
runforever :: Monad m => IntState m x -> m x
runforever state = do
    next <- step state
    case next of
        Left result -> return result
        Right newstate -> runforever newstate

-- Initial state of computation
makestate :: [Integer] -> x -> (Integer -> x -> m x) -> (x -> m (x, Integer)) -> IntState m x
makestate vals start w r = IntState
    (M.fromDistinctAscList $ zip [0..] vals)
    0
    0
    start
    w
    r
