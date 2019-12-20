import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad (guard)

data Asteroid = Asteroid {x :: Int, y :: Int} deriving (Eq, Ord)

data Direction = Direction {dx :: Int, dy :: Int} deriving (Eq, Ord)

getDirection :: Asteroid -> Asteroid -> Direction
getDirection a b = Direction (xx `div` gg) (yy `div` gg) where
    xx = x a - x b
    yy = y a - y b
    gg = gcd xx yy

type AsteroidField = Map Asteroid (Set Direction)

addAsteroid :: AsteroidField -> Asteroid -> AsteroidField
addAsteroid oldfield newaster = M.insert newaster visible updatedfield where
    updatedfield = M.mapWithKey (\aster dirs -> S.insert (getDirection newaster aster) dirs) oldfield
    visible = S.map (\aster -> getDirection aster newaster) $ M.keysSet oldfield

toAsteroids :: String -> [Asteroid]
toAsteroids str = do
    (ln, y) <- zip (lines str) [0..]
    do
        (c, x) <- zip ln [0..]
        guard $ c == '#'
        return $ Asteroid x y

main :: IO ()
main = interact $ show . maximum . fmap length . foldl addAsteroid M.empty . toAsteroids
