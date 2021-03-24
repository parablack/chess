module ParseFEN (
    initialFen,
    parseFen
) where

import Chess

import qualified Data.Char as Char
import qualified Data.Map as Map

initialFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

parsePos [x, y] = (Char.ord x - Char.ord 'a' + 1, read [y] :: Int)

parsePiece 'P' = Piece White Pawn
parsePiece 'N' = Piece White Knight
parsePiece 'B' = Piece White Bishop
parsePiece 'R' = Piece White Rook
parsePiece 'Q' = Piece White Queen
parsePiece 'K' = Piece White King
parsePiece 'p' = Piece Black Pawn
parsePiece 'n' = Piece Black Knight
parsePiece 'b' = Piece Black Bishop
parsePiece 'r' = Piece Black Rook
parsePiece 'q' = Piece Black Queen
parsePiece 'k' = Piece Black King

parseFenBoard board = parse board 1 8
    where parse [] _ _ = Map.empty
          parse (c:cs) x y
            | c == '/'        = parse cs 1 (y - 1)
            | Char.isDigit c  = parse cs (x + read [c]) y
            | otherwise       =
                Map.insert (x, y) (parsePiece c) $ parse cs (x + 1) y

parseFenTurn "w" = White
parseFenTurn "b" = Black

parseFenCastle = map castlePos
    where castlePos 'K' = (8, 1)
          castlePos 'Q' = (1, 1)
          castlePos 'k' = (8, 8)
          castlePos 'q' = (1, 8)

parseFenEnPassant "-" = Nothing
parseFenEnPassant pos = Just (parsePos pos)

parseFen :: String -> State
parseFen str =
    let board : turn : castle : enPassant : _ = words str
    in State (parseFenBoard board)
             (parseFenTurn turn)
             (parseFenCastle castle)
             (parseFenEnPassant enPassant)
