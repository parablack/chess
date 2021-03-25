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


findCorrespondingMove :: Board -> Piece -> Pos -> Pos -> Hopefully Move
findCorrespondingMove board piece@(Piece color Pawn) src@(x, y) dst@(x', y') =
    if abs (y - y') == 2 then
        return $ DoubleJump piece src dst
    else
        case posType color dst board of
            Capture -> return $ Jump piece src dst
            _       -> return $ EnPassant piece src dst (stepForward (inv color) dst)

findCorrespondingMove board piece@(Piece color King) src@(x, y) dst@(x', y') =
    if abs (x - x') == 2 then
        case x' of
            7 -> return $ Castle piece src dst (Piece color Rook) (8, y) (6, y)
            3 -> return $ Castle piece src dst (Piece color Rook) (1, y) (4, y)
            _ -> throwError (LogicError "Invalid castling destination or king moved 2 blocks")
    else
        return $ Jump piece src dst

findCorrespondingMove board piece src dst =
     return $ Jump piece src dst

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
                    ""  -> findCorrespondingMove board piece (xSrc, ySrc) (xDst, yDst)
                    "q" -> return $ Promotion (xSrc, ySrc) (xDst, yDst) (Piece (pieceColor piece) Queen)
                    "r" -> return $ Promotion (xSrc, ySrc) (xDst, yDst) (Piece (pieceColor piece) Rook)
                    "b" -> return $ Promotion (xSrc, ySrc) (xDst, yDst) (Piece (pieceColor piece) Bishop)
                    "n" -> return $ Promotion (xSrc, ySrc) (xDst, yDst) (Piece (pieceColor piece) Knight)
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


typeToIdentifier :: PieceType -> String
typeToIdentifier King = error "Promotion to King impossible"
typeToIdentifier Pawn = error "Promotion to Pawn impossible"
typeToIdentifier Queen  = "q"
typeToIdentifier Knight = "n"
typeToIdentifier Bishop = "b"
typeToIdentifier Rook   = "r"

moveToAN :: Move -> String
moveToAN Jump{moveSrc=src,moveDst=dst}       = serialize2Pos src dst
moveToAN DoubleJump{moveSrc=src,moveDst=dst} = serialize2Pos src dst
moveToAN Castle{kingSrc=src,kingDst=dst}     = serialize2Pos src dst
moveToAN EnPassant{moveSrc=src,moveDst=dst}  = serialize2Pos src dst
moveToAN Promotion{moveSrc=src,moveDst=dst,movePiece=piece}  =
    serialize2Pos src dst ++ typeToIdentifier (pieceType piece)

isANChar :: Char -> Bool
isANChar s = let ord = Data.Char.ord s
             in ord >= Data.Char.ord 'a' && ord <= Data.Char.ord 'h'

isANDigit :: Char -> Bool
isANDigit s = let ord = Data.Char.ord s
              in ord >= Data.Char.ord '1' && ord <= Data.Char.ord '8'


isAN :: String -> Bool
isAN (a:b:c:d:prom) = isANChar a && isANDigit b && isANChar c && isANDigit d &&
    case prom of
        "" -> True
        "q" -> True
        "r" -> True
        "b" -> True
        "n"  -> True
        _   -> False
isAN _ = False
