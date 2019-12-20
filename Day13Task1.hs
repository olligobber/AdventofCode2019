import IntCode

data InputState = NoInput | GotX Integer | GotXY Integer Integer

type ArcadeState = Integer

type WholeState = (InputState, ArcadeState)

updateInput :: Integer -> InputState -> (InputState, Maybe (Integer, Integer, Integer))
updateInput i NoInput = (GotX i, Nothing)
updateInput i (GotX x) = (GotXY x i, Nothing)
updateInput i (GotXY x y) = (NoInput, Just (x,y,i))

updateArcade :: (Integer, Integer, Integer) -> ArcadeState -> ArcadeState
updateArcade (_,_,2) i = i+1
updateArcade _ i = i

updateState :: Integer -> WholeState -> WholeState
updateState i (ins, ars) = case updateInput i ins of
    (newins, Nothing) -> (newins, ars)
    (newins, Just j) -> (newins, updateArcade j ars)

startInput :: InputState
startInput = NoInput

startArcade :: ArcadeState
startArcade = 0

startState :: WholeState
startState = (startInput, startArcade)

finishState :: WholeState -> Integer
finishState (_, a) = a

main :: IO ()
main = interact $ show . finishState . runSimple startState updateState undefined . readIntCode
