import Data.Set (Set)
import qualified Data.Set as S
import Data.Semigroup (Endo(..), stimes)
import Control.Monad (guard)

data Position = Position {x :: Int, y :: Int, d :: Int} deriving (Eq, Ord, Show)

neighbours :: Position -> Set Position
neighbours p = S.fromList $ do
    (dx,dy) <- [(0,1),(0,-1),(1,0),(-1,0)]
    case (x p + dx, y p + dy) of
        (-1,_) -> return $ Position 1 2 (d p - 1)
        (5,_) -> return $ Position 3 2 (d p - 1)
        (_,-1) -> return $ Position 2 1 (d p - 1)
        (_,5) -> return $ Position 2 3 (d p - 1)
        (2,2) -> do
            a <- [-2..2]
            return $ Position
                (2 - 2 * dx + a * dy)
                (2 - 2 * dy + a * dx)
                (d p + 1)
        _ -> return $ Position (x p + dx) (y p + dy) (d p)

type Bugs = Set Position

readBugs :: String -> Bugs
readBugs s = S.fromList $ do
    (ty,l) <- zip [0..4] (lines s)
    (tx,c) <- zip [0..4] l
    guard $ c == '#'
    return $ Position tx ty 0

updateBugs :: Endo Bugs
updateBugs = Endo $ \b -> S.fromList $ do
    p <- S.toList b ++ (S.toList b >>= S.toList . neighbours)
    if p `S.member` b then do
        guard $ length (b `S.intersection` neighbours p) == 1
        return p
    else do
        guard $ length (b `S.intersection` neighbours p) `elem` [1,2]
        return p

main :: IO ()
main = interact $ (++"\n") . show . length . appEndo (stimes 200 updateBugs) . readBugs
