import Chess
import qualified Data.Map as Map
import qualified Data.Char


findCorrespondingMove :: Piece -> Pos -> Pos -> Move
findCorrespondingMove piece src@(x, y) dst@(x', y') = case pieceType piece of
    Pawn -> if abs (y - y') == 2 then DoubleJump piece src dst else Jump piece src dst
    King -> if abs (x - x') == 2 then case x' of
        7 -> Castle piece src dst (Piece (pieceColor piece) Rook) (8, y) (6, y)
        3 -> Castle piece src dst (Piece (pieceColor piece) Rook) (1, y) (4, y)
        _ -> error "Invalid castling destination or king moved 2 blocks"
        else Jump piece src dst
    _ -> Jump piece src dst

parseSingleLAN :: Board -> String -> Move
parseSingleLAN board (x1 : y1 : x2 : y2 : _) =
    let
        xSrc = Data.Char.ord x1 - Data.Char.ord 'a' + 1
        ySrc = Data.Char.ord y1 - Data.Char.ord '0'
        xDst = Data.Char.ord x2 - Data.Char.ord 'a' + 1
        yDst = Data.Char.ord y2 - Data.Char.ord '0'
    in
        case Map.lookup (xSrc, ySrc) board of
            Just piece -> findCorrespondingMove piece (xSrc, ySrc) (xDst, yDst)
            _ -> error("Flobs parsing ist kaputt")

parseLANList :: State -> String -> State
parseLANList state "" = state
parseLANList state s =
    let board = stateBoard state
        move = parseSingleLAN board s
    in
    parseLANList (makeMove state move) (drop 5 s)

kiwipepe = "e2e4 h7h5 d2d4 h5h4 d4d5 h4h3 d1f3 g7g6 f1e2 f8g7 c1d2 e7e6 f3e3 g8f6 g1f3 g7f8 f3e5 f8g7 b1c3 b7b5 e2f1 b5b4 f1e2 b8c6 e2f1 c6a5 f1e2 a5c4 e2f1 c4b6 f1e2 c8a6 e2f1 d8e7 f1e2 a6c8 e2f1 c8b7 f1e2 b7a6 e3g3 a6b7 g3f3 b7a6"

kiwipepestate = parseLANList initialState kiwipepe
