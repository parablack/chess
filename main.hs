import ParseAN
import qualified ParseFEN
import Control.Monad
import Chess


reaction :: String -> State -> IO(State)
reaction "xboard" state = do
      putStrLn ""
      return state
reaction "new" state    = do
      putStrLn "# Starting new Game."
      return initialState
reaction an state
  | isAN an = do
      putStrLn "# Executing move"
      let newstate = applyANList state an
      print $ perft 1 newstate
      -- TODO answer
      return $! newstate
reaction _ state        = do
      putStrLn "# Unrecognized command."
      return state

--    "new" -> do
--        putStrLn "Hello!"
--        putStrLn "World!"
--        return initialState
--    a:b:c:d:prom:_
--    _ -> do
--        putStrLn "Command not recognized"
--        return state



loop :: State -> IO()
loop state = do
  line <- getLine
  let dispatch = head $ words line
  newstate <- reaction dispatch state
  loop newstate

main = loop initialState

-- main = do
--      print $ perft 0 initialState
--      print $ perft 1 initialState
--      print $ perft 2 initialState
--      print $ perft 3 initialState
--      print $ perft 4 initialState
--      print $ perft 5 initialState
--      print $ perft 6 initialState
--      print $ perft 7 initialState