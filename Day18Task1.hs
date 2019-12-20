import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Char (isUpper, isLower, toUpper)
import Control.Monad (guard)

data GridSquare = Wall | Corridor | POI PointOfInterest deriving Eq

data PointOfInterest = Door Char | Key Char | Start deriving (Eq, Ord, Show)

data Position = Position { x :: Int, y :: Int } deriving (Eq, Ord)

type Neighbours = Map PointOfInterest Int

type Graph = Map PointOfInterest Neighbours

type RGraph = Map PointOfInterest (Set PointOfInterest)

neighbours :: Position -> [Position]
neighbours p = do
    (dx,dy) <- [(0,1),(0,-1),(1,0),(-1,0)]
    return $ Position (x p + dx) (y p + dy)

fromPOI :: GridSquare -> Maybe PointOfInterest
fromPOI (POI p) = Just p
fromPOI _ = Nothing

showPOI :: PointOfInterest -> String
showPOI (Key c) = "K_"++[c]
showPOI (Door c) = "D_"++[c]
showPOI c = show c

isKey :: PointOfInterest -> Bool
isKey (Key _) = True
isKey _ = False

toSquare :: Char -> GridSquare
toSquare '#' = Wall
toSquare '.' = Corridor
toSquare '@' = POI Start
toSquare x
    | isLower x = POI $ Key $ toUpper x
    | isUpper x = POI $ Door x
    | otherwise = error $ "Unknown character in map: " ++ [x]

readMap :: String -> Map Position GridSquare
readMap m = M.fromList $ do
    (yord, l) <- zip [0..] $ lines m
    (xord, c) <- zip [0..] l
    return (Position xord yord, toSquare c)

data FloodState = FloodState {
    layer :: Int, -- Number of next layer
    unfilled :: Set Position, -- Positions in later layers
    goals :: Map Position PointOfInterest, -- Goals in later layers
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

floodFill :: Map Position GridSquare -> Position -> Neighbours
floodFill m p = floodFills $ FloodState
    1
    (M.keysSet $ M.filter (==Corridor) m)
    (M.delete p $ M.mapMaybe fromPOI m)
    (S.singleton p)
    M.empty

toGraph :: Map Position GridSquare -> Graph
toGraph m = M.fromList $ do
    (p, v) <- M.assocs $ M.mapMaybe fromPOI m
    return (v, floodFill m p)

-- Changes an undirected graph to a directed graph out from the given set
toRootedBy :: Set PointOfInterest -> Graph -> RGraph
toRootedBy s g
    | S.null s = M.keysSet <$> g
    | otherwise = toRootedBy nextLayer $ M.mapWithKey delBack g
    where
        nextLayer = foldl (\n v -> n `S.union` M.keysSet (g ! v)) S.empty s
        delBack v m
            | v `S.member` nextLayer = m `M.withoutKeys` s
            | otherwise = m

toRooted :: Graph -> RGraph
toRooted = toRootedBy $ S.singleton Start

-- Given a rooted graph, the current point and keys collected, output a set of deletable doors
getRedundentDoorsAt :: RGraph -> PointOfInterest -> Set Char -> Set Char
getRedundentDoorsAt g (Door c) keys
    | c `S.member` keys = foldl unionR (S.singleton c) (g ! Door c)
    | otherwise = foldl unionR S.empty (g ! Door c)
    where
        unionR d v = d `S.union` getRedundentDoorsAt g v keys
getRedundentDoorsAt g (Key c) keys = foldl unionR S.empty (g ! Key c) where
    unionR d v = d `S.union` getRedundentDoorsAt g v (c `S.insert` keys)
getRedundentDoorsAt g c keys = foldl unionR S.empty (g ! c) where
    unionR d v = d `S.union` getRedundentDoorsAt g v keys

getRedundentDoors :: RGraph -> Set Char
getRedundentDoors g = getRedundentDoorsAt g Start S.empty

canReachKey :: RGraph -> PointOfInterest -> Bool
canReachKey g p = any isKey (g ! p) || any (canReachKey g) (g ! p)

-- Given a rooted graph and the current point, output a set of keys that are on the path to other keys
getRedundentKeysAt :: RGraph -> PointOfInterest -> Set Char
getRedundentKeysAt g (Key c)
    | canReachKey g (Key c) = foldl unionR (S.singleton c) (g ! Key c)
    | otherwise = foldl unionR S.empty (g ! Key c)
    where
        unionR d v = d `S.union` getRedundentKeysAt g v
getRedundentKeysAt g c = foldl unionR S.empty (g ! c) where
    unionR d v = d `S.union` getRedundentKeysAt g v

getRedundentKeys :: RGraph -> Set Char
getRedundentKeys g = getRedundentDoors g `S.intersection` getRedundentKeysAt g Start

getRedundentNodes :: RGraph -> Set PointOfInterest
getRedundentNodes g = S.map Key (getRedundentKeys g) `S.union` S.map Door (getRedundentDoors g)

-- In some cases this will remove non-redundent nodes, but for this graph it doesn't
removeRedundentNodes :: Graph -> Graph
removeRedundentNodes g = foldr removePoint g $ getRedundentNodes $ toRooted g
-- The adjusted version of the input has had this minimisation and more applied to it

removePoint :: PointOfInterest -> Graph -> Graph
removePoint p g = M.mapWithKey updateNeighbours $ M.delete p g where
    updateNeighbours this n = M.unionWith min (M.delete p n) $ case M.lookup p n of
        Just dist -> fmap (+dist) $ M.delete this $ g!p
        Nothing -> M.empty

getKey :: Char -> Graph -> Graph
getKey c =
    M.mapKeys (\d -> if d == Key c then Start else d) .
    fmap (M.mapKeys (\d -> if d == Key c then Start else d)) .
    removePoint Start .
    removePoint (Door c)

data Walker = Walker {
    returnStates :: [(Graph, Int)],
    currentBest :: Maybe Int
}

startWalker :: Graph -> Walker
startWalker g = Walker [(g,0)] Nothing

stepWalker :: Walker -> Either Int Walker
stepWalker (Walker ((g,i):ss) Nothing) | length g == 1 = Right $ Walker ss (Just i)
stepWalker (Walker ((g,i):ss) (Just j)) | length g == 1 = j `seq` Right (Walker ss $ Just $ min i j)
stepWalker (Walker ((g,i):ss) j) = Right $ Walker (new++ss) j where
    new = do
        (Key nextKey, distance) <- M.assocs $ M.filterWithKey (const . isKey) $ g ! Start
        return (getKey nextKey g, i + distance)
stepWalker (Walker [] (Just i)) = Left i
stepWalker (Walker [] Nothing) = error "Walker failed"

completeWalker :: Walker -> Int
completeWalker w = case stepWalker w of
    Left i -> i
    Right n -> completeWalker n

shortestPath :: Graph -> Int
shortestPath = completeWalker . startWalker

main :: IO ()
main = interact $ (++"\n") . show . shortestPath . toGraph . readMap
