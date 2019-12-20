import Data.Set (Set)
import qualified Data.Set as S
import Data.Function (on)

data Direction = U | D | L | R deriving Read

data BoxPos = BoxPos { x :: Int, y :: Int } deriving (Eq, Ord)

type WireBox = Set BoxPos

type Distance = Maybe Int

updatePos :: BoxPos -> Direction -> BoxPos
updatePos p U = BoxPos (x p) (y p + 1)
updatePos p D = BoxPos (x p) (y p - 1)
updatePos p L = BoxPos (x p + 1) (y p)
updatePos p R = BoxPos (x p - 1) (y p)

distance :: BoxPos -> Int
distance p = ((+) `on` (abs . ($ p))) x y

updateDistance :: Distance -> BoxPos -> Distance
updateDistance Nothing b = Just $ distance b
updateDistance (Just d) b = Just $ min d $ distance b

startPos :: BoxPos
startPos = BoxPos 0 0

addFirstWire :: (WireBox, BoxPos) -> Direction -> (WireBox, BoxPos)
addFirstWire (b, p) d = (S.insert np b, np) where np = updatePos p d

startBox :: (WireBox, BoxPos)
startBox = (S.singleton startPos, startPos)

firstWire :: [Direction] -> WireBox
firstWire = fst . foldl addFirstWire startBox

addSecondWire :: WireBox -> (BoxPos, Distance) -> Direction -> (BoxPos, Distance)
addSecondWire b (p, h) d
    | np `S.member` b = (np, updateDistance h np)
    | otherwise     = (np, h)
    where
        np = updatePos p d

secondWire :: [Direction] -> WireBox -> Distance
secondWire ds b = snd $ foldl (addSecondWire b) (startPos, Nothing) ds

readDirection :: (String, String) -> [Direction]
readDirection (d,n) = replicate (read n) (read d)

readDirections :: String -> [Direction]
readDirections =
    concatMap (readDirection . splitAt 1) .
    words .
    (fmap $ \x -> if x == ',' then ' ' else x)

main :: IO ()
main = do
    firstWireBox <- firstWire . readDirections <$> getLine
    secondWireF <- secondWire . readDirections <$> getLine
    print $ secondWireF firstWireBox
