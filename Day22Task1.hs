import Data.Semigroup (Endo(..), Dual(..))
import Data.List (elemIndex)

stack :: Int
stack = 10007

readop :: Int -> String -> Endo Int
readop t s = Endo $ \n -> case words s of
    "deal":"into":_ -> (-1-n) `mod` t
    "deal":"with":"increment":x:_ -> (n * read x) `mod` t
    "cut":x:_ -> (n - read x) `mod` t

readshuffle :: Int -> String -> Endo Int
readshuffle t = getDual . foldMap (Dual . readop t) . lines

main :: IO ()
main = interact $ (++"\n") . show . flip appEndo 2019 . readshuffle stack
-- main = do
--     putStrLn "Stack:"
--     stack <- read <$> getLine
--     putStrLn "Steps:"
--     shuffle <- readshuffle stack <$> getContents
--     putStrLn $ unwords $ fmap show $ flip elemIndex (appEndo shuffle <$> [0..stack-1]) <$> [0..stack-1]
