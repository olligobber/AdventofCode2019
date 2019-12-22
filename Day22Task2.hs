{-# LANGUAGE DataKinds, TypeOperators #-}

import Data.Modular (ℤ, type (/)(), inv)
import Data.Semigroup (stimes)

type Card = ℤ / 119315717514047
-- type Card = ℤ / 10007

t :: Integer
t = 101741582076661

-- x => ax + b
data Linear = Linear { coefficient :: Card, constant :: Card }

-- composition left to right
instance Semigroup Linear where
    (Linear a b) <> (Linear c d) = Linear (a * c) (b * c + d)

instance Monoid Linear where
    mempty = Linear 1 0

app :: Linear -> Card -> Card
app (Linear a b) x = a*x+b

invert :: Linear -> Linear
invert (Linear a b) = Linear (inv a) (-b * inv a)

readop :: String -> Linear
readop s = case words s of
    "deal":"into":_ -> Linear (-1) (-1)
    "deal":"with":"increment":x:_ -> Linear (read x) 0
    "cut":x:_ -> Linear 1 (-read x)

readshuffle :: String -> Linear
readshuffle = foldMap readop . lines

main :: IO ()
main = interact $ (++"\n") . show . flip app 2020 . invert . stimes t . readshuffle
-- main = interact $ (++"\n") . show . flip app 2019 . readshuffle
