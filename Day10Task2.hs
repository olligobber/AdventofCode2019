import Data.Map (Map, (!))
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad (guard)
import Data.List (sortBy, sort, (!!))
import Data.Function (on)

data Asteroid = Asteroid {x :: Integer, y :: Integer} deriving (Eq, Ord, Show)

data Direction = Direction {dx :: Integer, dy :: Integer} deriving (Eq, Ord, Show)

-- gets direction from first to second
getDirection :: Asteroid -> Asteroid -> Direction
getDirection a b = Direction (xx `div` gg) (yy `div` gg) where
    xx = x b - x a
    yy = y b - y a
    gg = gcd xx yy

getAngle :: Direction -> Double
getAngle d = negate $ (atan2 `on` (fromInteger . ($ d))) dx dy

getDistance :: Asteroid -> Asteroid -> Integer
getDistance a b = (x a - x b)^2 + (y a - y b)^2

type AsteroidField = Map Asteroid (Set Direction)

addAsteroid :: AsteroidField -> Asteroid -> AsteroidField
addAsteroid oldfield newaster = M.insert newaster visible updatedfield where
    updatedfield = M.mapWithKey (\aster dirs -> S.insert (getDirection aster newaster) dirs) oldfield
    visible = S.map (\aster -> getDirection newaster aster) $ M.keysSet oldfield

toAsteroids :: String -> [Asteroid]
toAsteroids str = do
    (ln, y) <- zip (lines str) [0..]
    do
        (c, x) <- zip ln [0..]
        guard $ c == '#'
        return $ Asteroid x y

getBest :: AsteroidField -> Asteroid
getBest m = snd $ maximum $ (\(x,y) -> (length y,x)) <$> M.assocs m

laserOrderTagged :: AsteroidField -> [(Int, Double, Asteroid)]
laserOrderTagged m = sort asterstagged where
    laserpoint = getBest m
    asteroids = S.delete laserpoint $ M.keysSet m
    directions = (m ! laserpoint) :: Set Direction
    dirbuckets = (
        foldl
            (\ms a -> M.adjust (S.insert a) (getDirection laserpoint a) ms)
            (M.fromSet (const S.empty) directions)
            asteroids
        ) :: Map Direction (Set Asteroid)
    dirtagged = (
        (zip [0..] . sortBy (compare `on` getDistance laserpoint) . S.toList) <$> dirbuckets
        ) :: Map Direction [(Int, Asteroid)]
    asterstagged = (
        concat $
        M.elems $
        M.mapWithKey
            (\dir as -> fmap (\(dist, aster) -> (dist, getAngle dir, aster)) as)
            dirtagged
        ) :: [(Int, Double, Asteroid)]

laserOrder :: AsteroidField -> [Asteroid]
laserOrder = fmap (\(_,_,a) -> a) . laserOrderTagged

main :: IO ()
main = interact $ show . (\a -> x a * 100 + y a) . (!! 199) . laserOrder . foldl addAsteroid M.empty . toAsteroids
