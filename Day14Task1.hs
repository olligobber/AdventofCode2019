import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (elemIndex)
import qualified Data.Semigroup as Semigroup
import Data.Function (on)

mtimes :: (Integral b, Monoid a) => b -> a -> a
mtimes = Semigroup.mtimesDefault

newtype Ingredients = Ingredients {amounts :: Map String Integer} deriving Show

instance Semigroup Ingredients where
    a <> b = Ingredients $ (M.unionWith (+) `on` amounts) a b

instance Monoid Ingredients where
    mempty = Ingredients $ M.empty

type Recipes = Map String (Ingredients, Integer)

data Graph = Graph {vertices :: Set String, edges :: Set (String, String)}

readIngredient :: String -> Ingredients
readIngredient s = case words s of
    amount:name:_ -> Ingredients $ M.singleton name $ read amount
    _ -> error "Malformed recipe"

readRecipe :: String -> (String, (Ingredients, Integer))
readRecipe s = case (elemIndex ',' s, elemIndex '=' s) of
    (_, Nothing) -> error "Malformed recipe"
    (Nothing, Just i) -> case words (drop (i+3) s) of
        amount:name:_ -> (name, (readIngredient (take (i-1) s), read amount))
        _ -> error "Malformed recipe"
    (Just i, _) ->
        let (name, (others, amount)) = readRecipe (drop (i+2) s)
        in  (name, (others <> readIngredient (take i s), amount))

readRecipes :: String -> Recipes
readRecipes =
    M.fromListWith (error "Ingredient has multiple recipes") .
    fmap readRecipe .
    lines

fromEdges :: Set (String, String) -> Graph
fromEdges edges = Graph vertices edges where
    startVertices = S.map fst edges
    endVertices = S.map snd edges
    vertices = S.union startVertices endVertices

toGraph :: Recipes -> Graph
toGraph recipes = fromEdges . S.fromList $ do
    (end, (ingredients, _)) <- M.assocs recipes
    (start, _) <- M.assocs $ amounts ingredients
    return (start, end)

removeVertex :: String -> Graph -> Graph
removeVertex vertex graph = Graph
    (S.delete vertex $ vertices graph)
    (S.filter (\(start, end) -> start /= vertex && end /= vertex) $ edges graph)

-- Get a list where edges in the graph only travel backwards in the list
topoOrder :: Graph -> [String]
topoOrder graph
    | S.null (vertices graph) = []
    | otherwise = sink : topoOrder (removeVertex sink graph)
    where
        sink = case S.lookupMin sinks of
            Just s -> s
            Nothing -> error "Recipes do not form a DAG"
        sinks = S.filter noOutgoing $ vertices graph
        noOutgoing v = S.null $ S.filter ((== v) . fst) $ edges graph

eliminateIngredient :: Recipes -> Ingredients -> String -> Ingredients
eliminateIngredient recipes start name = case M.lookup name recipes of
    Nothing -> error $ "No recipe for " ++ name
    Just (requirement, increment) -> let
        target = M.findWithDefault 0 name $ amounts start
        without = Ingredients $ M.delete name $ amounts start
        times = case target `mod` increment of
            0 -> target `div` increment
            _ -> 1 + (target `div` increment)
        in (mtimes times requirement) <> without

eliminateAllBut :: Recipes -> Ingredients -> Ingredients
eliminateAllBut recipes start = foldl
    (eliminateIngredient recipes)
    start
    (init $ topoOrder $ toGraph recipes)

goal :: Ingredients
goal = Ingredients $ M.singleton "FUEL" 1

main :: IO ()
main = interact $ show . flip eliminateAllBut goal . readRecipes
