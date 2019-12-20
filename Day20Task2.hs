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

toPOI :: (Char, Char) -> Bool -> POI
toPOI ('A', 'A') _ = Start
toPOI ('Z', 'Z') _ = End
toPOI (x, y) b = Portal [x,y] b

startRead :: String -> Map Position Char
startRead s = M.fromList $ do
    (y,l) <- zip [0..] $ lines s
    (x,c) <- zip [0..] l
    return (Position x y, c)

isOuter :: Map Position Char -> Position -> Bool
isOuter m p = any id [
    x p == minX + 2,
    y p == minY + 2,
    x p == maxX - 2,
    y p == maxY - 2
    ] where
        minY = fst $ M.findMin $ M.mapKeys y m
        maxY = fst $ M.findMax $ M.mapKeys y m
        minX = fst $ M.findMin $ M.mapKeys x m
        maxX = fst $ M.findMax $ M.mapKeys x m

getPOIs :: Map Position Char -> Map Position POI
getPOIs m = M.fromList $ do
    (p, c) <- M.assocs $ M.filter isAlpha m
    np <- neighbours p
    Just e <- return $ M.lookup np m
    guard $ e == '.'
    op <- neighbours p
    Just d <- return $ flip M.lookup m op
    guard $ isAlpha d
    return (np, toPOI (flips (op<p) (c,d)) $ isOuter m np)
    where
        flips True (a,b) = (a,b)
        flips False (a,b) = (b,a)

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
    return $ (v, floodFill m p)

type Vertex = (Int, POI)

getNeighbours :: Graph -> Vertex -> Map Vertex Int
getNeighbours g (layer, p) = otherLayerNeighbours `M.union` thisLayerNeighbours where
    thisLayerNeighbours = M.mapKeys ((,) layer) $ g ! p
    otherLayerNeighbours = case (layer, p) of
        (0, Portal s True) -> M.empty
        (_, Portal s True) -> M.singleton (layer - 1, Portal s False) 1
        (_, Portal s False) -> M.singleton (layer + 1, Portal s True) 1
        _ -> M.empty

data Dijkstra = Dijkstra {
    toSearch :: OrdPSQ (Vertex,Int) Int (),
    found :: Set Vertex
} deriving Show

insert :: OrdPSQ (Vertex,Int) Int () -> (Vertex, Int) -> OrdPSQ (Vertex,Int) Int ()
insert q (k,p) = P.insert (k,p) p () q

stepDijkstra :: Graph -> Dijkstra -> Either Int Dijkstra
stepDijkstra g s = case fmap (\(a,_,_) -> a) $ P.findMin $ toSearch s of
    Just ((0,End),d) -> Left d
    Just (p, d) -> Right $
        if p `S.member` found s then Dijkstra
            (P.deleteMin $ toSearch s)
            (found s)
        else Dijkstra
            (foldl insert (P.deleteMin $ toSearch s) $ M.assocs $ (+d) <$> getNeighbours g p `M.withoutKeys` found s)
            (p `S.insert` found s)
    Nothing -> error "Dijkstra failed"

finishDijkstra :: Graph -> Dijkstra -> Int
finishDijkstra g s = case stepDijkstra g s of
    Left r -> r
    Right nextS -> finishDijkstra g nextS

finishDijkstraIO :: Graph -> Dijkstra -> IO Int
finishDijkstraIO g s = do
    putStrLn $
        "At node " ++
        show (fmap (\((a,_),_,_) -> a) $ P.findMin $ toSearch s) ++
        " with distance " ++
        show (fmap (\(_,a,_) -> a) $ P.findMin $ toSearch s)
    case stepDijkstra g s of
        Left r -> r <$ putStrLn "Done"
        Right nextS -> finishDijkstraIO g nextS

shortestPath :: Graph -> Int
shortestPath g = finishDijkstra g $ Dijkstra (P.singleton ((0,Start),0) 0 ()) S.empty

shortestPathIO :: Graph -> IO ()
shortestPathIO g = finishDijkstraIO g (Dijkstra (P.singleton ((0,Start),0) 0 ()) S.empty) >>= print

main :: IO ()
main = interact $ (++"\n") . show . shortestPath . toGraph . startRead
-- main = getContents >>= shortestPathIO . toGraph . startRead
