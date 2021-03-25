import ParseAN
import qualified ParseFEN
import Heuristics
import Control.Monad
import Chess
import Control.Monad.Except
import qualified Control.Monad.State.Lazy as SMonad
import System.IO
import Data.Maybe

-- geklaut von Flow by flo
infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

data XBoardData = XBoardData {
      xboardForces :: Bool,
      xboardState :: State
}
type XBoardState = SMonad.StateT XBoardData IO


printMoveError :: MoveError -> String -> IO()
printMoveError (ParserError e) move = putStrLn $ "Illegal Move (" ++ e ++ "): " ++ move
printMoveError (LogicError e)  move = putStrLn $ "Illegal Move (" ++ e ++ "): " ++ move

println :: String -> XBoardState ()
println = lift . putStrLn

get :: (XBoardData -> a) -> XBoardState a
get property = fmap property SMonad.get

initialXBoardData :: XBoardData
initialXBoardData = XBoardData {xboardForces=False, xboardState=initialState}


simpleMinMax :: Int -> State -> Int
simpleMinMax 0 state = getScore (inv $ stateTurn state) state
simpleMinMax depth state =
      let
            moves = legalMoves state
      in
            if null moves then
                  -99999999
            else
                  moves |> map (negate . simpleMinMax (depth - 1) . makeMove state)
                        |> maximum

getMove :: State -> Move
getMove state = legalMoves state
            |>  map (\move -> (simpleMinMax 3 (makeMove state move), move))
            -- |> map (\move -> (getScore (stateTurn state) (makeMove state move), move))
            |> maximum
            |> snd

makeNextMove :: XBoardState ()
makeNextMove =
      do
            state <- get xboardState
            let   ourMove = getMove state
                  alteredState = makeMove state ourMove
            SMonad.modify (\x -> x {xboardState=alteredState})
            println $ "move " ++ moveToAN ourMove

reaction :: [String] -> XBoardState ()
reaction ["xboard"] = do
      println ""

reaction ["new"] = do
      println "Starting new Game 6969."
      SMonad.put initialXBoardData
      println "The program destroyes you!"

reaction ["protover", "2"] = do
      println "feature usermove=1 sigint=0 sigterm=0 time=0 debug=1 done=1"

reaction ["usermove", an]
  | isAN an   = do
      println "# Executing move"
      state <- get xboardState
      case applyANList state an of
            Left e -> do lift $ printMoveError e an
            Right newstate -> do
                  SMonad.modify (\x -> x {xboardState=newstate})
                  makeNextMove
  | otherwise = do
      println $ "Illegal Move (move is not in AN format) " ++ an

reaction [command,  _]  = do
      println $ "Error (unknown command): " ++ command

reaction [] = return ()

reaction _  = do
    println "# Not Implemented!"


loop :: XBoardState ()
loop = do
      line <- lift getLine
      case words line of
            ["quit"] -> return ()
            dispatch -> do
                  reaction dispatch
                  loop

main = do
  putStrLn "Initializing engine..."
  hSetBuffering stdout NoBuffering

  SMonad.runStateT loop initialXBoardData
