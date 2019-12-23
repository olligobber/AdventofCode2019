import IntCode
import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as V
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad (forM, forever)
import Control.Concurrent (forkIO)
import System.Exit (exitSuccess)

type Cat6 = MVar [(Integer, Integer)]

newCat6 :: IO Cat6
newCat6 = V.newMVar []

readCat6 :: Cat6 -> IO (Maybe (Integer, Integer))
readCat6 = flip V.modifyMVar (\l -> case l of
        x:xs -> return $ (xs, Just x)
        [] -> return $ ([], Nothing)
    )

writeCat6 :: Cat6 -> (Integer, Integer) -> IO ()
writeCat6 c v = V.modifyMVar_ c (return . (++[v]))

type Network = Map Int Cat6

makeNetwork :: IO Network
makeNetwork = M.fromListWith (error "Duplicate Key") <$>
    forM [0..49] (\i -> (,) i <$> newCat6)

writeNetwork :: Network -> Int -> (Integer, Integer) -> IO ()
writeNetwork n a v = case M.lookup a n of
    Just chan -> writeCat6 chan v
    Nothing -> error "No channel with this address"

addOutput :: Network -> IO (Network, Cat6)
addOutput net = do
    outChan <- newCat6
    return (
        M.insertWith (error "Output already here") 255 outChan net,
        outChan)

data IOController = IOController {
    myChan :: Cat6,
    network :: Network,
    unsent :: Maybe (Int, Maybe Integer),
    unreceived :: Maybe Integer
}

newIOController :: Network -> Int -> IOController
newIOController chans myid = case M.lookup myid chans of
    Just chan -> IOController chan chans Nothing (Just $ toInteger myid)
    Nothing -> error "No channel for this controller"

readIOController :: IOController -> IO (IOController, Integer)
readIOController cont = case unreceived cont of
    Just i -> return (
        IOController (myChan cont) (network cont) (unsent cont) Nothing,
        i)
    Nothing -> do
        r <- readCat6 $ myChan cont
        case r of
            Just (a,b) -> return (
                IOController (myChan cont) (network cont) (unsent cont) (Just b),
                a)
            Nothing -> return (
                cont,
                -1)

writeIOController :: Integer -> IOController -> IO IOController
writeIOController v cont = case unsent cont of
    Just (a, Just b) -> do
        writeNetwork (network cont) a (b,v)
        return $
            IOController (myChan cont) (network cont) Nothing (unreceived cont)
    Just (a, Nothing) -> return $
        IOController (myChan cont) (network cont) (Just (a, Just v)) (unreceived cont)
    Nothing -> return $
        IOController (myChan cont) (network cont) (Just (fromInteger v, Nothing)) (unreceived cont)

runIOController :: [Integer] -> IOController -> IO ()
runIOController intcode cont = () <$ (forkIO $
    () <$ run cont writeIOController readIOController intcode)

main :: IO ()
main = do
    intcode <- readIntCode <$> readFile "Day23Input"
    (network, output) <- makeNetwork >>= addOutput
    mapM_ (runIOController intcode) $ newIOController network <$> [0..49]
    forever $ do
        v <- readCat6 output
        case v of
            Nothing -> return ()
            Just (x,y) -> print y >> exitSuccess
