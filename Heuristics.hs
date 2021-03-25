module Heuristics (
    getScore
) where

import Chess
import qualified Data.Map as Map

pieceValue :: PieceType -> Int
pieceValue Pawn = 100
pieceValue Knight = 300
pieceValue Bishop = 325
pieceValue Rook = 500
pieceValue Queen = 900
pieceValue King = 0

weightPieces :: State -> Color -> Int
weightPieces state color = sum (map (\(pos, piece) -> pieceValue $ pieceType piece) (filter (\(pos, piece) -> pieceColor piece == color) (Map.assocs (stateBoard state))))

getScoreWhite :: State -> Int
getScoreWhite state = weightPieces state White - weightPieces state Black

getScore :: Color -> State -> Int
getScore White = getScoreWhite
getScore Black = negate . getScoreWhite