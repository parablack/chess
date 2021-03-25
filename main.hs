import ParseAN
import qualified ParseFEN
import Control.Monad
import Chess
import Control.Monad.Except
import qualified Control.Monad.State.Lazy as SMonad
import System.IO


data XBoardData = XBoardData {
      xboardForces :: Bool,
      xboardState :: State
}
type XBoardState = SMonad.StateT XBoardData IO


printMoveError :: MoveError -> String -> IO()
printMoveError (ParserError e) move = putStrLn $ "Illegal Move (" ++ e ++ "): " ++ move
printMoveError (LogicError e)  move = putStrLn $ "Illegal Move (" ++ e ++ "): " ++ move

println :: String -> XBoardState()
println = lift . putStrLn

getBoardState :: XBoardState State
getBoardState = fmap xboardState SMonad.get

makeNextMove :: XBoardState ()
makeNextMove =
      do
            state <- getBoardState
            let   ourMove = head $ legalMoves state
                  alteredState = makeMove state ourMove
            SMonad.modify (\x -> x {xboardState=alteredState})
            println $ "move " ++ moveToAN ourMove

-- dispatch, first argument
reaction :: String -> String -> XBoardState ()
reaction "xboard"  _ = do
      println ""
reaction "new"     _ = do
      println "Starting new Game."
      SMonad.put (XBoardData {xboardForces=False, xboardState=initialState})
      println "The program destroyes you!"
reaction "protover" "2" = do
      println "feature usermove=1 sigint=0 sigterm=0 time=0 debug=1 done=1"
reaction "usermove" an
  | isAN an   = do
      println "# Executing move"
      state <- getBoardState
      case applyANList state an of
            Left e -> do lift $ printMoveError e an
            Right newstate -> do
                  SMonad.modify (\x -> x {xboardState=newstate})
                  makeNextMove
  | otherwise = println $ "Illegal Move (move is not in AN format) " ++ an
reaction command  _  = do
      println $ "Error (unknown command): " ++ command

--    "new" -> do
--        putStrLn "Hello!"
--        putStrLn "World!"
--        return initialState
--    a:b:c:d:prom:_
--    _ -> do
--        putStrLn "Command not recognized"
--        return state



loop :: XBoardState ()
loop = do
      line <- lift getLine

      let dispatch = head $ words line
      let arg = head $ tail $ words line
      -- putStrLn $ "Got dispatch keyword: " ++ dispatch
      case dispatch of
            "quit" -> return ()
            dispatch -> do
                  reaction dispatch arg
                  loop

main = do
  putStrLn "Initializing engine..."
  hSetBuffering stdout NoBuffering

  SMonad.runStateT loop (XBoardData {xboardForces=False, xboardState=initialState})

-- main = do
--      print $ perft 0 initialState
--      print $ perft 1 initialState
--      print $ perft 2 initialState
--      print $ perft 3 initialState
--      print $ perft 4 initialState
--      print $ perft 5 initialState
--      print $ perft 6 initialState
--      print $ perft 7 initialState
