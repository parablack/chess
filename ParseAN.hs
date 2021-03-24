module ParseAN (
    applyANList,
    parseSingleAN,
    moveToAN,
    isAN,
    MoveError(..),
    Hopefully
) where

import Chess
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char

import Control.Monad.Except

data MoveError = ParserError String | LogicError String
type Hopefully = Either MoveError

findCorrespondingMove :: Piece -> Pos -> Pos -> Hopefully Move
findCorrespondingMove piece src@(x, y) dst@(x', y') =
    case pieceType piece of
        Pawn -> if abs (y - y') == 2
            then return $ DoubleJump piece src dst
            else return $ Jump piece src dst
        King -> if abs (x - x') == 2 then case x' of
            7 -> return $ Castle piece src dst (Piece (pieceColor piece) Rook) (8, y) (6, y)
            3 -> return $ Castle piece src dst (Piece (pieceColor piece) Rook) (1, y) (4, y)
            _ -> throwError (LogicError "Invalid castling destination or king moved 2 blocks")
            else return $ Jump piece src dst
        _ -> return $ Jump piece src dst

-- pieceTypeFromChar :: Char -> PieceType

parseSingleAN :: Board -> String -> Hopefully Move
parseSingleAN board (x1 : y1 : x2 : y2 : promo) =
    let
        xSrc = Data.Char.ord x1 - Data.Char.ord 'a' + 1
        ySrc = Data.Char.ord y1 - Data.Char.ord '0'
        xDst = Data.Char.ord x2 - Data.Char.ord 'a' + 1
        yDst = Data.Char.ord y2 - Data.Char.ord '0'
    in
        case Map.lookup (xSrc, ySrc) board of
            Just piece ->
                case promo of
                    ""  -> findCorrespondingMove piece (xSrc, ySrc) (xDst, yDst)
                    "q" -> return $ Promotion (xSrc, ySrc) (xDst, yDst) (Piece (pieceColor piece) Queen)
                    _   ->  throwError (ParserError "AN too long")
            _ -> throwError (ParserError "Malformed AN or no figure on source square")



applyANList :: State -> String -> Hopefully State
applyANList state  =
    let
        process state [] = return state
        process state (w:ws) = do
            move <- parseSingleAN (stateBoard state) w
            if isValidMove state move then
                process (makeMove state move) ws
            else throwError (LogicError "Move is illegal on current board")
    in
        process state . words

serializePos :: Pos -> String
serializePos (x, y) =
    let xRepr = Data.Char.chr (x + Data.Char.ord 'a' - 1)
        yRepr = Data.Char.chr (y + Data.Char.ord '0')
    in
        [xRepr, yRepr]

serialize2Pos :: Pos -> Pos -> String
serialize2Pos p1 p2 = serializePos p1 ++ serializePos p2

moveToAN :: Move -> String
moveToAN Jump{moveSrc=src,moveDst=dst}       = serialize2Pos src dst
moveToAN DoubleJump{moveSrc=src,moveDst=dst} = serialize2Pos src dst
moveToAN Castle{kingSrc=src,kingDst=dst}   = serialize2Pos src dst
moveToAN EnPassant{moveSrc=src,moveDst=dst}  = serialize2Pos src dst
moveToAN Promotion{moveSrc=src,moveDst=dst}  = serialize2Pos src dst ++ "q" -- TODO other pieces?

isANChar :: Char -> Bool
isANChar s = let ord = Data.Char.ord s in ord >= Data.Char.ord 'a' && ord <= Data.Char.ord 'h'
isANDigit :: Char -> Bool
isANDigit s = let ord = Data.Char.ord s in ord >= Data.Char.ord '1' && ord <= Data.Char.ord '8'


isAN :: String -> Bool
isAN (a:b:c:d:prom) = isANChar a && isANDigit b && isANChar c && isANDigit d &&
    case prom of
        "q" -> True
        "Q" -> True
        ""  -> True
        _   -> False
isAN _ = False

kiwipepe = "e2e4 h7h5 d2d4 h5h4 d4d5 h4h3 d1f3 g7g6 f1e2 f8g7 c1d2 e7e6 f3e3 g8f6 g1f3 g7f8 f3e5 f8g7 b1c3 b7b5 e2f1 b5b4 f1e2 b8c6 e2f1 c6a5 f1e2 a5c4 e2f1 c4b6 f1e2 c8a6 e2f1 d8e7 f1e2 a6c8 e2f1 c8b7 f1e2 b7a6 e3g3 a6b7 g3f3 b7a6"

kiwipepestate = applyANList initialState kiwipepe
