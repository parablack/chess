module Chess (
    Color(..),
    Piece(..),
    PieceType(..),
    Pos,
    Board(..),
    State(..),
    Move(..),
    makeMove,
    legalMoves,
    perft,
    initialState,
    isValidMove,
    isChecked,
    isCheckmate,
    inv
) where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char

data Color = Black | White
  deriving (Eq, Show, Ord)

inv :: Color -> Color
inv Black = White
inv White = Black

data PieceType = Pawn | Knight | Rook | Bishop | Queen | King
    deriving (Eq, Ord)

instance Show PieceType where
    show Pawn = "P"
    show Knight = "N"
    show Rook = "R"
    show Bishop = "B"
    show Queen = "Q"
    show King = "K"

data Piece = Piece {
    pieceColor :: Color,
    pieceType  :: PieceType
} deriving (Eq, Ord)

instance Show Piece where
    show (Piece White pieceType) = map Char.toUpper (show pieceType)
    show (Piece Black pieceType) = map Char.toLower (show pieceType)

type Pos = (Int, Int)
type Offset = (Int, Int)
type Board = (Map.Map Pos Piece)

{-

Board Coordinates xy

18 28 38 48 58 68 78 88
17 27 37 47 57 57 77 87
16 26 36 46 56 56 76 86
15 25 35 45 55 55 75 85
14 24 34 44 54 54 74 84
13 23 33 43 53 53 73 83
12 22 32 42 52 52 72 82
11 21 31 41 51 61 71 81

-}

onBoard :: Pos -> Bool
onBoard (x, y) = 1 <= x && x <= 8 && 1 <= y && y <= 8

findPiece :: Piece -> Board -> [Pos]
findPiece piece board = map fst $ filter ((==piece) . snd) $ Map.assocs board

data PosType = Empty | Invaild | Blocked | Capture

posType :: Color -> Pos -> Board -> PosType
posType color pos board
    | onBoard pos = case Map.lookup pos board of
        Nothing               -> Empty
        Just (Piece color' _) ->
            if color == color' then Blocked else Capture
    | otherwise = Invaild

enumPattern :: Color -> Pos -> Board -> [Offset] -> [Pos]
enumPattern _ _ _ [] = []
enumPattern color start@(x, y) board ((dx, dy) : patterns) =
        case posType color pos board of
            Empty    -> pos : moves
            Capture  -> pos : moves
            _        -> moves
        where
            pos = (x + dx, y + dy)
            moves = enumPattern color start board patterns

enumLine :: Color -> Pos -> Board -> Offset -> [Pos]
enumLine color start board (dx, dy) = step (next start)
    where
        next (x, y) = (x + dx, y + dy)
        step pos = case posType color pos board of
            Empty    -> pos : step (next pos)
            Capture  -> [pos]
            _        -> []

data Move = Jump {movePiece :: Piece, moveSrc :: Pos, moveDst :: Pos}
          | DoubleJump {movePiece :: Piece, moveSrc :: Pos, moveDst :: Pos} -- pawn moved twice
          | Promotion {moveSrc :: Pos, moveDst :: Pos, movePiece :: Piece}
          | Castle {kingPiece :: Piece, kingSrc :: Pos, kingDst :: Pos,
                    rookPiece :: Piece, rookSrc :: Pos, rookDst :: Pos}
          | EnPassant {movePiece :: Piece, moveSrc :: Pos, moveDst :: Pos, attackedPos :: Pos}
           -- piece, start, destination, attacked position
          deriving (Eq, Show, Ord)

applyMove :: Move -> Board -> Board
applyMove (Jump piece src dst)  =
    Map.insert dst piece . Map.delete src

applyMove (DoubleJump piece src dst) =
    applyMove (Jump piece src dst)

applyMove (Promotion src dst piece)  =
    applyMove (Jump piece src dst)

applyMove (Castle king kingsrc kingdst rook rooksrc rookdst)  =
    applyMove (Jump king kingsrc kingdst) .
    applyMove (Jump rook rooksrc rookdst)

applyMove (EnPassant piece src dst attacked) =
    Map.insert dst piece . Map.delete src . Map.delete attacked


patternMoves :: [Offset] -> Piece -> Pos -> Board -> [Move]
patternMoves pattern piece pos board =
    map (Jump piece pos) $ enumPattern (pieceColor piece) pos board pattern

linesMoves :: [Offset] -> Piece -> Pos -> Board -> [Move]
linesMoves dirs piece pos board =
    map (Jump piece pos) $ concatMap (enumLine (pieceColor piece) pos board) dirs

stepForward :: Color -> Pos -> Pos
stepForward Black (x, y) = (x, y - 1)
stepForward White (x, y) = (x, y + 1)

promoteIfPossible :: Move -> [Move]
promoteIfPossible (Jump piece@(Piece Black Pawn) src dst@(_, 1)) =
    map (Promotion src dst . Piece Black) [Knight, Rook, Bishop, Queen]

promoteIfPossible (Jump piece@(Piece White Pawn) src dst@(_, 8)) =
    map (Promotion src dst . Piece White) [Knight, Rook, Bishop, Queen]

promoteIfPossible move = [move]

isPawnOnStartRow :: Piece -> Pos -> Bool
isPawnOnStartRow (Piece Black Pawn) (_, 7) = True
isPawnOnStartRow (Piece White Pawn) (_, 2) = True
isPawnOnStartRow _ _ = False

pawnBoardMoves :: Piece -> Pos -> Board -> [Move]
pawnBoardMoves piece start board =
    concatMap promoteIfPossible $
        step (x, y) ++ capture (x-1, y) ++ capture (x+1, y) ++ doublestep (x, y)
    where
        color = pieceColor piece
        (x, y) = stepForward color start
        step pos = case posType color pos board of
            Empty   -> [Jump piece start pos]
            _       -> []
        capture pos = case posType color pos board of
            Capture -> [Jump piece start pos]
            _       -> []
        doublestep pos =
            let
                doubleStepPos = stepForward color pos
            in
                [DoubleJump piece start doubleStepPos |
                isPawnOnStartRow piece start
                && fieldListEmpty board [pos, doubleStepPos]]

knightPattern =
    [(-2, 1), (-1, 2), (1, 2), (2, 1), (-2, -1), (-1, -2), (1, -2), (2, -1)]
kingPattern =
    [(-1, 1), (0, 1), (1, 1), (-1, 0), (1, 0), (-1, -1), (0, -1), (1, -1)]
rookLines   = [(1, 0), (-1, 0), (0, 1), (0, -1)]
bishopLines = [(1, 1), (1, -1), (-1, 1), (-1, -1)]
queenLines  = rookLines ++ bishopLines

boardMoves :: Piece -> Pos -> Board -> [Move]
boardMoves piece@(Piece _ Knight) = patternMoves knightPattern piece
boardMoves piece@(Piece _ Rook)   = linesMoves rookLines piece
boardMoves piece@(Piece _ Bishop) = linesMoves bishopLines piece
boardMoves piece@(Piece _ Queen)  = linesMoves queenLines piece
boardMoves piece@(Piece _ King)   = patternMoves kingPattern piece
boardMoves piece@(Piece _ Pawn)   = pawnBoardMoves piece

allBoardMoves :: Color -> Board -> [Move]
allBoardMoves color board =
    concat [boardMoves piece pos board | (pos, piece) <- fields]
    where
        fields = filter  ((== color) . pieceColor . snd) $ Map.assocs board

isBadPos :: (Piece -> Bool) -> Board -> Pos -> Bool
isBadPos isBad board pos =
    maybe False isBad (Map.lookup pos board)

-- for color  at position on board, is there an enemy piece in range [Offset] fulfilling isBad
patternAttacked :: Color -> Pos -> Board -> [Offset] -> (Piece -> Bool) -> Bool
patternAttacked color start board pattern isBad =
    any (isBadPos isBad board) (enumPattern color start board pattern)

linesAttacked :: Color -> Pos -> Board -> [Offset] -> (Piece -> Bool) -> Bool
linesAttacked color start board dirs isBad =
    any (attacked . enumLine color start board) dirs
    where
        attacked [] = False
        attacked ps = isBadPos isBad board $ last ps

pawnAttacked :: Color -> Pos -> Board -> Bool
pawnAttacked color pos board  =
    patternAttacked color next board pattern (== Piece (inv color) Pawn)
    where
        next = stepForward color pos
        pattern = [(-1, 0), (1, 0)]

boardAttacked :: Color -> Pos -> Board -> Bool
boardAttacked color pos board =
    patternAttacked color pos board knightPattern (== Piece badcolor Knight) ||
    patternAttacked color pos board kingPattern   (== Piece badcolor King)   ||
    linesAttacked   color pos board rookLines     rookLike                   ||
    linesAttacked   color pos board bishopLines   bishopLike                 ||
    pawnAttacked    color pos board
    where
        badcolor = inv color
        rookLike   (Piece color' Rook)   = badcolor == color'
        rookLike   (Piece color' Queen)  = badcolor == color'
        rookLike   _                     = False
        bishopLike (Piece color' Bishop) = badcolor == color'
        bishopLike (Piece color' Queen)  = badcolor == color'
        bishopLike _                     = False

checked :: Color -> Board -> Bool
checked color board =
    let piece = Piece color King
    in  case findPiece piece board of
            [pos]  -> boardAttacked color pos board
            _      -> error "no unique king on board!"


data State = State {
    stateBoard       :: Board,
    stateTurn        :: Color,
    stateCastle      :: [Pos],      -- rook positions
    stateEnPassant   :: Maybe Pos
} deriving (Eq)


-- TODO: display is right but row, col is getting mixed up
pieceAscii row col board = maybe " " show (Map.lookup (col, row) board)
rowOfPices row board = concat [" " ++ pieceAscii row i board |i<-[1..8]]

instance Show State where
    show (State board turn _ _) = "Turn is " ++ show turn ++ "\n"
                                    ++ "  +-----------------+\n"
                                    ++ concat [show (9-i) ++ " |" ++ rowOfPices (9-i) board ++ " |\n"|i<-[1..8]]
                                    ++ "  +-----------------+\n"
                                    ++ "    a b c d e f g h"


updateCastleCapture :: Move -> [Pos] -> [Pos]
updateCastleCapture move rooks =
    case move of
        Jump _ _ pos2 -> List.delete pos2 rooks
        Promotion _ pos2 _ -> List.delete pos2 rooks
        _ -> rooks

updateCastleMovement :: Move -> [Pos] -> [Pos]
updateCastleMovement Castle{kingSrc=(_, y)}        = List.delete (1, y) . List.delete (8, y)
updateCastleMovement (Jump (Piece White King) _ _) = List.delete (1, 1) . List.delete (8, 1)
updateCastleMovement (Jump (Piece Black King) _ _) = List.delete (1, 8) . List.delete (8, 8)
updateCastleMovement (Jump _ pos1 _)               = List.delete pos1
updateCastleMovement _                             = id


isValidMove :: State -> Move -> Bool
isValidMove state move = elem move $ legalMoves state

-- asserts that move is valid
makeMove :: State -> Move -> State
makeMove (State board turn castle enPassant) move =
    State (applyMove move board)
          (inv turn)
          (updateCastleMovement move $ updateCastleCapture move castle)       -- TODO check if correct
          enPassantPos
    where
        enPassantPos = case move of
            DoubleJump _ _ dst -> Just dst
            _ -> Nothing

enPassantAttackers :: Board -> Color -> Pos -> Pos -> [Move]
enPassantAttackers board color attackedPos attackerPos =
    case Map.lookup attackerPos board of
        Just piece@(Piece color' Pawn) ->
            let destination = stepForward color attackedPos in
                [EnPassant piece attackerPos destination attackedPos
                    | color' == color]
        _ -> []

enPassantMoves :: Color -> State -> [Move]
enPassantMoves color state =
    case stateEnPassant state of
        Nothing -> []
        Just attackedPos@(x, y) ->
            enpassantChecker attackedPos (x + 1, y) ++
            enpassantChecker attackedPos (x - 1, y)
    where
        board = stateBoard state
        enpassantChecker = enPassantAttackers board color

-- checks if all fields for castling are empty.
fieldListEmpty :: Board -> [Pos] -> Bool
fieldListEmpty board = all (\x -> Map.lookup x board == Nothing)

-- checks if the two fields for castling are not checked.
fieldListNotChecked :: Board -> Color -> [Pos] -> Bool
fieldListNotChecked board color =
    all (\x -> not $ boardAttacked color x board)

emptyListForRook :: Pos -> [Pos]
emptyListForRook (1, x) = [(2, x), (3, x), (4, x)]
emptyListForRook (8, x) = [(6, x), (7, x)]

notCheckedListForRook :: Pos -> [Pos]
notCheckedListForRook (1, x) = [(5, x), (3, x), (4, x)]
notCheckedListForRook (8, x) = [(5, x), (6, x), (7, x)]

createCastleMove :: Color -> Pos -> Move
createCastleMove color (1, y) = Castle (Piece color King) (5, y) (3, y) (Piece color Rook) (1, y) (4, y)
createCastleMove color (8, y) = Castle (Piece color King) (5, y) (7, y) (Piece color Rook) (8, y) (6, y)

-- on board, for color and never-moved rook on pos, castling move or nothing.
castlingPossibleForRook :: Board -> Color -> Pos -> [Move]
castlingPossibleForRook board color pos =
    [createCastleMove color pos |
        fieldListEmpty board (emptyListForRook pos)
     && fieldListNotChecked board color (notCheckedListForRook pos)]

allCastlingMoves :: State -> [Move]
allCastlingMoves State{stateBoard=board, stateTurn=color, stateCastle=rooks} =
    concatMap (castlingPossibleForRook board color) rooks

legalMoves :: State -> [Move]
legalMoves state =
    filter (not . checked color . stateBoard . makeMove state) moves
    where
        color = stateTurn state
        moves = allBoardMoves color (stateBoard state)
                ++ enPassantMoves color state
                ++ allCastlingMoves state

isChecked :: State -> Bool
isChecked state = checked (stateTurn state) (stateBoard state)

isCheckmate :: State -> Bool  -- mflo approved
isCheckmate state = isChecked state && legalMoves state == []

initialBoard = Map.fromList (
    [((i, 8), Piece Black pieceType) | (i, pieceType) <- zip [1..8] order] ++
    [((i, 7), Piece Black Pawn)      | i <- [1..8]                       ] ++
    [((i, 2), Piece White Pawn)      | i <- [1..8]                       ] ++
    [((i, 1), Piece White pieceType) | (i, pieceType) <- zip [1..8] order] )
    where
        order = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

fperft :: (State -> Int) -> Int -> State -> Int
fperft f n state
    | n < 0  = 0
    | n == 0 = f state
    | n > 0  =
        sum $ map (fperft f (n - 1) . makeMove state) $ legalMoves state

perft :: Int -> State -> Int
perft = fperft (const 1)

perftChecked :: Int -> State -> Int
perftChecked = fperft (\state -> if isChecked state then 1 else 0)

perftCheckmate :: Int -> State -> Int
perftCheckmate = fperft (\state -> if isCheckmate state then 1 else 0)

perftEnPassant :: Int -> State -> Int
perftEnPassant n =
    let
        countMove EnPassant {} = 1
        countMove _            = 0
    in
        fperft (sum . map countMove . legalMoves) (n - 1)

initialState = State initialBoard White [(1, 1), (8, 1), (1, 8), (8, 8)] Nothing
