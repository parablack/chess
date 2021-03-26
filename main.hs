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


{-
  Note: This is bad:
  depth=4, fen="rnbqkbnr/pp2p1p1/8/3p1p1p/3B4/1P4P1/P1P1PPBP/RN1QK1NR b KQkq - 0 6"
-}

simpleMinMax :: Int -> State -> (Int, [Move])
simpleMinMax 0 state = (getScore (stateTurn state) state, [])
simpleMinMax depth state =
  let moves = legalMoves state
   in if null moves
        then -- if isInCheck 0> checkmate
        -- else => stalemate
            if checked (stateTurn state) (stateBoard state) then
            (-100000, [])
            else
            (0, [])
        else
          moves
            |> map
              ( \move ->
                  let (score, moves) = simpleMinMax (depth - 1) (makeMove state move)
                   in (negate score, move : moves)
              )
            |> maximum


alphaBeta :: Int -> State -> Int -> Int -> (Int, [Move])
alphaBeta 0 state _ _ = (getScore (stateTurn state) state, [])
alphaBeta depth state alpha beta =
  let moves = legalMoves state
   in if null moves
        then -- if isInCheck 0> checkmate
        -- else => stalemate
            if checked (stateTurn state) (stateBoard state) then
            (-100000, [])
            else
            (0, [])
        else let
              loop (move : otherMoves) maxWert =
                  let (nscore, moves) = alphaBeta (depth - 1) (makeMove state move) (-beta) (-maxWert)
                      score = negate nscore
                      thisMove = (score, move : moves)
                  in
                  if score > maxWert then
                        if maxWert >= beta then
                              thisMove
                        else
                              let otherBestMove@(otherScore, _) = loop otherMoves score
                              in if otherScore > score then otherBestMove else thisMove
                  else
                        loop otherMoves maxWert
              loop [] maxWert = (-100000000, [])
             in
                  loop moves alpha



getMove :: State -> Maybe Move
getMove state = case snd (alphaBeta 5 state (-100000000) 100000000) of
                  (x : _) -> Just x
                  [] -> Nothing

makeNextMove :: XBoardState ()
makeNextMove =
      do
            state <- get xboardState
            case getMove state of
                  Just ourMove -> do
                                    let alteredState = makeMove state ourMove
                                    SMonad.modify (\x -> x {xboardState=alteredState})
                                    println $ "move " ++ moveToAN ourMove
                  Nothing -> do println "offer draw"

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
                  skipMove <- get xboardForces
                  unless skipMove makeNextMove

  | otherwise = do
      println $ "Illegal Move (move is not in AN format) " ++ an

reaction ["force"] = do
      println "# Switching to forced mode"
      SMonad.modify (\x -> x {xboardForces = True})

reaction ["go"] = do
      println "# Going!"
      SMonad.modify (\x -> x {xboardForces = False})
      makeNextMove

reaction ["rejected", s] = do
      println "# Thanks for nothing..."
reaction ["accepted", _] = return ()
reaction ["easy", _] = return ()
reaction ["post", _] = return ()
reaction ("level" : _) = do
      println "# Timing control is unlucky"
      return ()



reaction (command : _)  = do
      println $ "Error (unknown command): " ++ command

reaction [] = return ()




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
