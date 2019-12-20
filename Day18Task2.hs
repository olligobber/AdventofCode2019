import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Char (isUpper, isLower, toUpper, isNumber)
import Control.Monad (guard)

data GridSquare = Wall | Corridor | POI PointOfInterest deriving Eq

data PointOfInterest = Door Char | Key Char | Bot Int deriving (Eq, Ord, Show)

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
showPOI (Bot c) = "B_"++show c

isKey :: PointOfInterest -> Bool
isKey (Key _) = True
isKey _ = False

toSquare :: Char -> GridSquare
toSquare '#' = Wall
toSquare '.' = Corridor
toSquare x
    | isNumber x = POI $ Bot (read [x])
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

removePoint :: PointOfInterest -> Graph -> Graph
removePoint p g = M.mapWithKey updateNeighbours $ M.delete p g where
    updateNeighbours this n = M.unionWith min (M.delete p n) $ case M.lookup p n of
        Just dist -> fmap (+dist) $ M.delete this $ g!p
        Nothing -> M.empty

getKey :: Int -> Char -> Graph -> Graph
getKey i c =
    M.mapKeys (\d -> if d == Key c then Bot i else d) .
    fmap (M.mapKeys (\d -> if d == Key c then Bot i else d)) .
    removePoint (Bot i) .
    removePoint (Door c)

bestBot3 :: Int -> Graph -> (Int, Graph)
bestBot3 i g
    | length (g ! Bot 3) == 0 = (i,g)
    | otherwise = minimum $ do
        (Key nextKey, distance) <- M.assocs $ M.filterWithKey (const . isKey) $ g ! Bot 3
        return $ bestBot3 (i + distance) (getKey 3 nextKey g)

data Walker = Walker {
    returnStates :: [(Graph, Int)],
    currentBest :: Maybe Int
}

startWalker :: Graph -> Walker
startWalker g = Walker [(h,i)] Nothing where
    (i,h) = bestBot3 0 g

stepWalker :: Walker -> Either Int Walker
stepWalker (Walker ((g,i):ss) Nothing) | length g == 4 = Right $ Walker ss (Just i)
stepWalker (Walker ((g,i):ss) (Just j)) | length g == 4 = j `seq` Right (Walker ss $ Just $ min i j)
stepWalker (Walker ((g,i):ss) j) = Right $ Walker (new++ss) j where
    new = do
        botnum <- [1..4]
        (Key nextKey, distance) <- M.assocs $ M.filterWithKey (const . isKey) $ g ! Bot botnum
        return (getKey botnum nextKey g, i + distance)
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
