import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as M
import Data.Semigroup (Sum(..))
import Data.List (elemIndex)
import Data.Maybe (fromJust)

type Tree = Map String String

addNode :: Tree -> (String, String) -> Tree
addNode t (c, p) = M.insert c p t

ancestors :: Tree -> String -> Sum Int
ancestors _ "COM" = Sum 0
ancestors t n = Sum 1 <> ancestors t (t ! n)

orbits :: Tree -> Int
orbits t = getSum $ foldMap (ancestors t) $ M.keys t

readOrbit :: String -> (String, String)
readOrbit s = (drop 4 s, take 3 s)

main :: IO ()
main = interact $ show . orbits . foldl addNode M.empty . fmap readOrbit . lines
