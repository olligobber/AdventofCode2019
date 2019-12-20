import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as M
import Data.List ()

type Tree = Map String String

addNode :: Tree -> (String, String) -> Tree
addNode t (c, p) = M.insert c p t

countAncestors :: Tree -> String -> Int
countAncestors _ "COM" = 0
countAncestors t n = 1 + countAncestors t (t ! n)

getAncestors :: Tree -> String -> [String]
getAncestors _ "COM" = ["COM"]
getAncestors t n = n : getAncestors t (t ! n)

commonAncestor :: Tree -> String -> String -> String
commonAncestor t x y = let
    ax = reverse $ getAncestors t x
    ay = reverse $ getAncestors t y
    commonChain = takeWhile (uncurry (==)) $ zip ax ay
    in fst $ last commonChain

distance :: Tree -> String -> String -> Int
distance t x y = countAncestors t x + countAncestors t y - 2 * countAncestors t (commonAncestor t x y) - 2

readOrbit :: String -> (String, String)
readOrbit s = (drop 4 s, take 3 s)

main :: IO ()
main = interact $ show . (\t -> distance t "YOU" "SAN") . foldl addNode M.empty . fmap readOrbit . lines
