import IntCode
import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as V
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad (forM, when)
import Control.Concurrent (forkIO)
import System.Exit (exitSuccess)

data Cat6 = Cat6 {
    unread :: [(Integer, Integer)],
    timesEmpty :: Integer
}

newCat6 :: Cat6
newCat6 = Cat6 [] 0

readCat6 :: Cat6 -> (Cat6, Maybe (Integer, Integer))
readCat6 c = case unread c of
    [] -> (Cat6 [] (timesEmpty c + 1), Nothing)
    (x:xs) -> (Cat6 xs 0, Just x)

writeCat6 :: Cat6 -> (Integer, Integer) -> Cat6
writeCat6 c v = Cat6 (unread c ++ [v]) 0

type Network = MVar (Map Int Cat6)

makeNetwork :: IO Network
makeNetwork = V.newMVar $ M.fromListWith (error "Duplicate Key") $
    zip (255:[0..49]) (repeat newCat6)

readNetwork :: Network -> Int -> IO (Maybe (Integer, Integer))
readNetwork net a = V.modifyMVar net $ \m ->
    case readCat6 <$> M.lookup a m of
        Just (c,v) -> return (M.insert a c m, v)
        Nothing -> error "No channel with this address"

writeNetwork :: Network -> Int -> (Integer, Integer) -> IO ()
writeNetwork net a v = V.modifyMVar_ net $ \m -> case M.lookup a m of
    Just c -> return $ M.insert a (writeCat6 c v) m
    Nothing -> error "No channel with this address"

isBlocked :: Network -> IO Bool
isBlocked net = all ((>2) . timesEmpty) <$> V.readMVar net

data NatController = NatController {
    net :: Network,
    lastReceived :: Maybe (Integer, Integer),
    lastSent :: Maybe (Integer, Integer)
}

makeNatController :: Network -> NatController
makeNatController net = NatController net Nothing Nothing

readNatController :: NatController -> IO NatController
readNatController cont = do
    v <- readNetwork (net cont) 255
    case v of
        Just vs -> return $ NatController (net cont) (Just vs) (lastSent cont)
        Nothing -> return cont

runNatController :: NatController -> IO ()
runNatController c = do
    cont <- readNatController c
    block <- isBlocked (net cont)
    if block then
        case (lastReceived cont, lastReceived cont == lastSent cont) of
            (Just (x,y), True) -> print y
            (Just v, False) -> do
                writeNetwork (net cont) 0 v
                runNatController $
                    NatController (net cont) (Just v) (Just v)
            (Nothing, _) -> runNatController cont
    else
        runNatController cont

data IOController = IOController {
    myID :: Int,
    network :: Network,
    unsent :: Maybe (Int, Maybe Integer),
    unreceived :: Maybe Integer
}

newIOController :: Network -> Int -> IOController
newIOController net myid =
    IOController myid net Nothing (Just $ toInteger myid)

readIOController :: IOController -> IO (IOController, Integer)
readIOController cont = case unreceived cont of
    Just i -> return (
        IOController (myID cont) (network cont) (unsent cont) Nothing,
        i)
    Nothing -> do
        r <- readNetwork (network cont) (myID cont)
        case r of
            Just (a,b) -> return (
                IOController (myID cont) (network cont) (unsent cont) (Just b),
                a)
            Nothing -> return (
                cont,
                -1)

writeIOController :: Integer -> IOController -> IO IOController
writeIOController v cont = case unsent cont of
    Just (a, Just b) -> do
        writeNetwork (network cont) a (b,v)
        return $
            IOController (myID cont) (network cont) Nothing (unreceived cont)
    Just (a, Nothing) -> return $
        IOController (myID cont) (network cont) (Just (a, Just v)) (unreceived cont)
    Nothing -> return $
        IOController (myID cont) (network cont) (Just (fromInteger v, Nothing)) (unreceived cont)

runIOController :: [Integer] -> IOController -> IO ()
runIOController intcode cont = () <$ (forkIO $
    () <$ run cont writeIOController readIOController intcode)

main :: IO ()
main = do
    intcode <- readIntCode <$> readFile "Day23Input"
    network <- makeNetwork
    mapM_ (runIOController intcode) $ newIOController network <$> [0..49]
    runNatController $ makeNatController network
