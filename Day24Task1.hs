import Data.Set (Set)
import qualified Data.Set as S
import Data.Semigroup (Sum(..))
import Control.Monad (guard)

data Position = Position {x :: Int, y :: Int} deriving (Eq, Ord)

neighbours :: Position -> Set Position
neighbours p = S.fromList $ do
    (dx,dy) <- [(0,1),(0,-1),(1,0),(-1,0)]
    return $ Position (x p + dx) (y p + dy)

allPositions :: [Position]
allPositions = Position <$> [0..4] <*> [0..4]

hashPos :: Position -> Integer
hashPos p = 2 ^ (x p + 5 * y p)

type Bugs = Set Position

readBugs :: String -> Bugs
readBugs s = S.fromList $ do
    (ty,l) <- zip [0..4] (lines s)
    (tx,c) <- zip [0..4] l
    guard $ c == '#'
    return $ Position tx ty

updateBugs :: Bugs -> Bugs
updateBugs b = S.fromList $ do
    p <- allPositions
    if p `S.member` b then do
        guard $ length (b `S.intersection` neighbours p) == 1
        return p
    else do
        guard $ length (b `S.intersection` neighbours p) `elem` [1,2]
        return p

hashBugs :: Bugs -> Integer
hashBugs = getSum . foldMap (Sum . hashPos)

getBugRepeat :: Set Integer -> Bugs -> Integer
getBugRepeat s b
    | hashBugs b `S.member` s = hashBugs b
    | otherwise = getBugRepeat (hashBugs b `S.insert` s) (updateBugs b)

main :: IO ()
main = interact $ (++"\n") . show . getBugRepeat S.empty . readBugs
