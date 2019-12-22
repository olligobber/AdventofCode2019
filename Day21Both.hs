import IntCode
import Control.Monad (forever)

main :: IO ()
main = do
    intcode <- readIntCode <$> readFile "Day21Input"
    forever $ do
        putStrLn "Enter code:"
        code <-
            (++repeat '\n') .
            fmap (\x -> if x == ',' then '\n' else x) <$> getLine
        runSimpleRead
            code
            (\x s ->
                if x < 128 then
                    s <$ (putChar $ toEnum $ fromInteger x)
                else
                    s <$ (putStrLn "" >> print x)
                )
            (\(x:xs) -> (xs, toInteger $ fromEnum x))
            intcode
