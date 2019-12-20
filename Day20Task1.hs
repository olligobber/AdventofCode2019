import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as P
import Data.Char (isAlpha)
import Control.Monad (guard)

data Position = Position {x :: Int, y :: Int} deriving (Eq, Ord, Show)

neighbours :: Position -> [Position]
neighbours p = do
    (dx,dy) <- [(0,1),(0,-1),(1,0),(-1,0)]
    return $ Position (x p + dx) (y p + dy)

data POI = Start | End | Portal String Bool deriving (Eq, Ord, Show)

showPOI :: POI -> String
showPOI (Portal x b) = "P_" ++ x ++ show (if b then 1 else 0)
showPOI x = show x

toPOI :: (Char, Char) -> POI
toPOI ('A', 'A') = Start
toPOI ('Z', 'Z') = End
toPOI (x, y) = Portal [max x y, min x y] True

getOtherPortal :: POI -> Neighbours
getOtherPortal (Portal x b) = M.singleton (Portal x $ not b) 1
getOtherPortal _ = M.empty

startRead :: String -> Map Position Char
startRead s = M.fromList $ do
    (y,l) <- zip [0..] $ lines s
    (x,c) <- zip [0..] l
    return (Position x y, c)

getPOIs :: Map Position Char -> Map Position POI
getPOIs m = snd $ M.mapAccum updatePortals S.empty $ M.fromList $ do
    (p, c) <- M.assocs $ M.filter isAlpha m
    np <- neighbours p
    Just e <- return $ M.lookup np m
    guard $ e == '.'
    Just d <- flip M.lookup m <$> neighbours p
    guard $ isAlpha d
    return (np, toPOI (c,d))
    where
        updatePortals set (Portal s _) = (s `S.insert` set, Portal s $ s `S.member` set)
        updatePortals set x = (set, x)

getTerrain :: Map Position Char -> Set Position
getTerrain = M.keysSet . M.filter (=='.')

type Neighbours = Map POI Int

type Graph = Map POI Neighbours

data FloodState = FloodState {
    layer :: Int, -- Number of next layer
    unfilled :: Set Position, -- Positions in later layers
    goals :: Map Position POI, -- Goals in later layers
    toFill :: Set Position, -- Positions in this layer
    achieved :: Neighbours -- Goals in this and previous layers
}

floodFills :: FloodState -> Neighbours
floodFills s
    | S.null (toFill s) = achieved s
    | otherwise = floodFills $ FloodState
        (layer s + 1)
        (unfilled s `S.difference` fillNeighbours)
        (goals s `M.withoutKeys` fillNeighbours)
        (unfilled s `S.intersection` fillNeighbours)
        (M.fromList (flip zip (repeat $ layer s) $
            M.elems $ goals s `M.restrictKeys` fillNeighbours)
        `M.union` achieved s)
    where
        fillNeighbours = S.fromList $ concatMap neighbours $ S.toList $ toFill s

floodFill :: Map Position Char -> Position -> Neighbours
floodFill m p = floodFills $ FloodState
    1
    (S.delete p $ getTerrain m)
    (M.delete p $ getPOIs m)
    (S.singleton p)
    M.empty

toGraph :: Map Position Char -> Graph
toGraph m = M.fromList $ do
    (p, v) <- M.assocs $ getPOIs m
    return $ (v, floodFill m p <> getOtherPortal v)

data Dijkstra = Dijkstra {
    toSearch :: OrdPSQ (POI,Int) Int POI,
    found :: Set POI
} deriving Show

insert :: OrdPSQ (POI,Int) Int POI -> (POI, Int) -> OrdPSQ (POI,Int) Int POI
insert q (k,p) = P.insert (k,p) p k q

stepDijkstra :: Graph -> Dijkstra -> Either Int Dijkstra
stepDijkstra g s = case P.findMin $ toSearch s of
    Just (_, d, End) -> Left d
    Just (_, d, p) -> Right $
        if p `S.member` found s then Dijkstra
            (P.deleteMin $ toSearch s)
            (found s)
        else Dijkstra
            (foldl insert (P.deleteMin $ toSearch s) $ M.assocs $ (+d) <$> (g ! p) `M.withoutKeys` found s)
            (p `S.insert` found s)
    Nothing -> error "Dijkstra failed"

finishDijkstra :: Graph -> Dijkstra -> Int
finishDijkstra g s = case stepDijkstra g s of
    Left r -> r
    Right nextS -> finishDijkstra g nextS

shortestPath :: Graph -> Int
shortestPath g = finishDijkstra g $ Dijkstra (P.singleton (Start,0) 0 Start) S.empty

main :: IO ()
main = interact $ (++"\n") . show . shortestPath . toGraph . startRead
