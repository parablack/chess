import qualified Data.Map as Map

data Color = Black | White
  deriving (Eq, Show)

inv :: Color -> Color
inv Black = White
inv White = Black

data PieceType = Pawn | Knight | Rook | Bishop | Queen | King
    deriving (Eq, Show)

data Piece = Piece {
    pieceColor :: Color,
    pieceType  :: PieceType
} deriving (Eq, Show)

type Pos = (Int, Int)
type Offset = (Int, Int)
type Board = Map.Map Pos Piece

{-

Board Coordinates xy

18 28 38 42 58 58 78 88
17 27 37 42 57 57 77 87
16 26 36 42 56 56 76 86
15 25 35 42 55 55 75 85
14 24 34 42 54 54 74 84
13 23 33 42 53 53 73 83
12 22 32 42 52 52 72 82
11 21 31 41 51 61 71 81

-}

onBoard :: Pos -> Bool
onBoard (x, y) = 1 <= x && x <= 8 && 1 <= y && y <= 8

findPiece :: Piece -> Board -> [Pos]
findPiece piece board = map fst $ filter ((==piece) . snd) $ Map.assocs board

data PosType = Empty | Invaild | Blocked | Capture

posType :: Piece -> Pos -> Board -> PosType
posType (Piece color _) pos board
    | onBoard pos = case Map.lookup pos board of
        Nothing               -> Empty
        Just (Piece color' _) ->
            if color == color' then Blocked else Capture
    | otherwise = Invaild

enumPattern :: Piece -> Pos -> Board -> [Offset] -> [Pos]
enumPattern _ _ _ [] = []
enumPattern piece start@(x, y) board ((dx, dy) : patterns) =
        case posType piece pos board of
            Empty    -> pos : moves
            Capture  -> pos : moves
            _        -> moves
        where
            pos = (x + dx, y + dy)
            moves = enumPattern piece start board patterns

enumLine :: Piece -> Pos -> Board -> Offset -> [Pos]
enumLine piece start board (dx, dy) = step (next start)
    where
        next (x, y) = (x + dx, y + dy)
        step pos = case posType piece pos board of
            Empty    -> pos : step (next pos)
            Capture  -> [pos]
            _        -> []

data Move = Jump Piece Pos Pos
          | Promotion Pos Pos Piece
          | Castle
          | EnPassant Piece Pos Pos Pos -- piece, start, destination, attacked position
          deriving (Eq, Show)

applyMove :: Move -> Board -> Board
applyMove (Jump piece src dst) board =
    Map.insert dst piece $ Map.delete src $ board

applyMove (Promotion src dst piece) board =
    Map.insert dst piece $ Map.delete src $ board

applyMove (Castle) board = error "castle not implemented!"
applyMove (EnPassant piece src dst attacked) board =
    Map.insert dst piece $ Map.delete src $ Map.delete attacked $ board


patternMoves :: [Offset] -> Piece -> Pos -> Board -> [Move]
patternMoves pattern piece pos board =
    map (Jump piece pos) $ enumPattern piece pos board pattern

linesMoves :: [Offset] -> Piece -> Pos -> Board -> [Move]
linesMoves dirs piece pos board =
    map (Jump piece pos) $ concatMap (enumLine piece pos board) $ dirs

stepForward :: Piece -> Pos -> Pos
stepForward (Piece Black _) (x, y) = (x, y - 1)
stepForward (Piece White _) (x, y) = (x, y + 1)

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
        (step (x, y)) ++ (capture (x-1, y)) ++ (capture (x+1, y)) ++ (doublestep (x, y))
    where
        (x, y) = stepForward piece start
        step pos = case posType piece pos board of
            Empty   -> [Jump piece start pos]
            _       -> []
        capture pos = case posType piece pos board of
            Capture -> [Jump piece start pos]
            _       -> []
        doublestep pos = if isPawnOnStartRow piece start
            then case step pos of
                []      -> []
                _       -> step $ stepForward piece pos
            else []

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
    case Map.lookup pos board of
        Nothing      -> False
        Just piece   -> isBad piece

patternAttacked :: Piece -> Pos -> Board -> [Offset] -> (Piece -> Bool) -> Bool
patternAttacked piece start board pattern isBad =
    any (isBadPos isBad board) (enumPattern piece start board pattern)

linesAttacked :: Piece -> Pos -> Board -> [Offset] -> (Piece -> Bool) -> Bool
linesAttacked piece start board dirs isBad =
    any (attacked . enumLine piece start board) dirs
    where
        attacked [] = False
        attacked ps = isBadPos isBad board $ last ps

pawnAttacked :: Piece -> Pos -> Board -> Color -> Bool
pawnAttacked piece pos board color =
    patternAttacked piece next board pattern (==(Piece color Pawn))
    where
        next = stepForward piece pos
        pattern = [(-1, 0), (1, 0)]

boardAttacked :: Piece -> Pos -> Board -> Bool
boardAttacked piece pos board =
    patternAttacked piece pos board knightPattern (==(Piece color Knight))  ||
    patternAttacked piece pos board kingPattern   (==(Piece color King))    ||
    linesAttacked   piece pos board rookLines     rookLike                  ||
    linesAttacked   piece pos board bishopLines   bishopLike                ||
    pawnAttacked    piece pos board color
    where
        color = inv (pieceColor piece)
        rookLike   (Piece color' Rook)  = (color == color')
        rookLike   (Piece color' Queen) = (color == color')
        rookLike   _                = False
        bishopLike (Piece color' Bishop) = (color == color')
        bishopLike (Piece color' Queen)  = (color == color')
        bishopLike _                = False

checked :: Color -> Board -> Bool
checked color board =
    case findPiece piece board of
        [pos]  -> boardAttacked piece pos board
        _      -> error "no unique king on board!"
    where
        piece = (Piece color King)

data State = State {
    stateBoard       :: Board,
    stateTurn        :: Color,
    stateCastle      :: [Pos],      -- rook positions
    stateEnPassant   :: Maybe Pos
} deriving (Eq, Show)

makeMove :: State -> Move -> State
makeMove (State board turn castle enPassant) move =
    State (applyMove move board)
          (inv turn)
          []       -- TODO implement castle
          enPassantPos
    where
        enPassantPos = case move of
            Jump piece@(Piece _ Pawn) (x, y) dst@(x', y') ->
                if abs (y' - y) == 2 then Just dst else Nothing
            _  -> Nothing

enPassantAttackers :: Board -> Color -> Pos -> Pos -> [Move]
enPassantAttackers board color attackedPos attackerPos =
    case Map.lookup attackerPos board of
        Just piece@(Piece color' Pawn) ->
            if color' == color
                then [EnPassant piece attackerPos
                        (stepForward piece attackedPos) attackedPos]
                else []
        _ -> []

enPassantMoves :: Color -> State -> [Move]
enPassantMoves color state =
    case stateEnPassant state of
        Nothing -> []
        Just attackedPos@(x, y) ->
            (enpassantChecker attackedPos (x + 1, y)) ++
            (enpassantChecker attackedPos (x - 1, y))
    where
        board = stateBoard state
        enpassantChecker = enPassantAttackers board color

legalMoves :: State -> [Move]
legalMoves state =
    filter (not . checked color . stateBoard . makeMove state) moves
    where
        color = stateTurn state
        moves = allBoardMoves color (stateBoard state)
                ++ enPassantMoves color state

emptyBoard = Map.fromList (
    [((i, 8), Piece Black pieceType) | (i, pieceType) <- zip [1..8] order] ++
    [((i, 7), Piece Black Pawn)      | i <- [1..8]                       ] ++
    [((i, 2), Piece White Pawn)      | i <- [1..8]                       ] ++
    [((i, 1), Piece White pieceType) | (i, pieceType) <- zip [1..8] order] )
    where
        order = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

perft :: Int -> State -> Int
perft 0 state = 1
perft n state  = sum $ map (perft (n - 1) . makeMove state) $ legalMoves state

initialState = State emptyBoard White [] Nothing

main = do
     putStrLn $ show $ perft 0 initialState
     putStrLn $ show $ perft 1 initialState
     putStrLn $ show $ perft 2 initialState
     putStrLn $ show $ perft 3 initialState
     putStrLn $ show $ perft 4 initialState
     putStrLn $ show $ perft 5 initialState
     putStrLn $ show $ perft 6 initialState
