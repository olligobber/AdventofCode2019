import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Function (on)

data Direction = U | D | L | R deriving Read

data BoxPos = BoxPos { x :: Int, y :: Int } deriving (Eq, Ord)

type WireBox = Map BoxPos Int

type Distance = Maybe Int

updatePos :: BoxPos -> Direction -> BoxPos
updatePos p U = BoxPos (x p) (y p + 1)
updatePos p D = BoxPos (x p) (y p - 1)
updatePos p L = BoxPos (x p + 1) (y p)
updatePos p R = BoxPos (x p - 1) (y p)

updateDistance :: Distance -> Int -> Distance
updateDistance Nothing i = Just i
updateDistance (Just d) i = Just $ min d i

startPos :: BoxPos
startPos = BoxPos 0 0

addFirstWire :: (WireBox, BoxPos, Int) -> Direction -> (WireBox, BoxPos, Int)
addFirstWire (b, p, i) d = (M.insert np (i+1) b, np, i+1) where np = updatePos p d

firstWire :: [Direction] -> WireBox
firstWire ds = let (b,_,_) = foldl addFirstWire (M.empty, startPos, 0) ds in b

addSecondWire :: WireBox -> (BoxPos, Distance, Int) -> Direction -> (BoxPos, Distance, Int)
addSecondWire b (p, h, i) d = case M.lookup np b of
    Just j -> (np, updateDistance h (j+i+1), i+1)
    Nothing -> (np, h, i+1)
    where
        np = updatePos p d

secondWire :: [Direction] -> WireBox -> Distance
secondWire ds b = let (_,d,_) = foldl (addSecondWire b) (startPos, Nothing, 0) ds in d

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
