import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map, (!))

data State = State {
    values :: Map Int Int,
    pointer :: Int
    }

run :: State -> Either Int State
run state = case values state ! pointer state of
    99 -> Left $ values state ! 0
    1 -> Right $ State (M.insert output (values state ! input1 + values state ! input2) $ values state) (pointer state + 4)
    2 -> Right $ State (M.insert output (values state ! input1 * values state ! input2) $ values state) (pointer state + 4)
    _ -> error "Undefined opcode"
    where
        opcode = values state ! pointer state
        input1 = values state ! (pointer state + 1)
        input2 = values state ! (pointer state + 2)
        output = values state ! (pointer state + 3)

runforever :: State -> Int
runforever state = case run state of
    Left result -> result
    Right newstate -> runforever newstate

makeState :: [Int] -> State
makeState vals = State (M.fromDistinctAscList $ zip [0..] vals) 0

fixState :: State -> State
fixState state = State (M.insert 1 12 . M.insert 2 2 $ values state) (pointer state)

main :: IO ()
main = interact $ show . runforever . fixState . makeState . fmap read . words . fmap (\x -> if x == ',' then ' ' else x)
