import Data.Semigroup (Endo(..), stimes)
import Data.Function (on)

newtype ModTen = ModTen { fromModTen :: Int }

instance Num ModTen where
    a + b = ModTen $ flip mod 10 $ ((+) `on` fromModTen) a b
    a * b = ModTen $ flip mod 10 $ ((*) `on` fromModTen) a b
    abs = id
    signum = id
    fromInteger = ModTen . flip mod 10 . fromInteger
    negate = ModTen . flip mod 10 . negate . fromModTen

instance Show ModTen where
    show = show . fromModTen

readString :: String -> [ModTen]
readString = fmap (fromInteger . read . return) . head . words

repeatList :: Int -> [x] -> [x]
repeatList i = concat . replicate i

-- Works correctly on second half of input, even if some of start is missing
smartPhase :: [ModTen] -> [ModTen]
smartPhase = scanr1 (+)

smartPhaseTimes :: Int -> [ModTen] -> [ModTen]
smartPhaseTimes i = appEndo $ stimes i $ Endo smartPhase

main :: IO ()
main = interact $
    (++"\n") .
    concatMap show .
    take 8 .
    smartPhaseTimes 100 .
    drop (5975677 - 9193*650) .
    repeatList (10^4 - 9193) .
    readString
