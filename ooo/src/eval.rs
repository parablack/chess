use chess::{BitBoard, Board, BoardStatus, ChessMove, Color, File, Piece, NUM_COLORS};
use smallvec::SmallVec;

use crate::piece_tables::*;

#[derive(Debug, Clone)]
pub struct AuxState {
    last_moves: SmallVec<[ChessMove; 4]>,
    has_castled_ooo: [bool; NUM_COLORS],
}

impl AuxState {
    pub fn new() -> Self {
        AuxState {
            last_moves: SmallVec::new(),
            has_castled_ooo: [false; NUM_COLORS],
        }
    }
    pub fn update(&self, board: &Board, m: ChessMove) -> Self {
        let AuxState {
            mut last_moves,
            mut has_castled_ooo,
        } = self.clone();
        if last_moves.len() == last_moves.inline_size() {
            last_moves.remove(0);
        }
        last_moves.push(m);

        if board.king_square(board.side_to_move()) == m.get_source()
            && board.my_castle_rights().has_queenside()
            && m.get_dest().get_file() == File::C
        {
            has_castled_ooo[board.side_to_move().to_index()] = true;
        }
        AuxState {
            last_moves,
            has_castled_ooo,
        }
    }

    pub fn is_draw(&self) -> bool {
        self.last_moves.len() >= 4
            && self.last_moves[0].get_source() == self.last_moves[2].get_dest()
            && self.last_moves[0].get_dest() == self.last_moves[2].get_source()
            && self.last_moves[1].get_source() == self.last_moves[3].get_dest()
            && self.last_moves[1].get_dest() == self.last_moves[3].get_source()
    }
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq)]
pub struct Score(pub i32);

impl Score {
    pub fn flip(&self) -> Self {
        Score(-self.0)
    }

    pub fn best() -> Self {
        Score(i32::MAX / 2)
    }

    pub fn worst() -> Self {
        Score(i32::MIN / 2)
    }

    pub fn very_bad() -> Self {
        Score(i32::MIN / 4)
    }
}

fn weight_pieces(board: Board, mask: BitBoard, flip: bool) -> i32 {
    let pawns = board.pieces(Piece::Pawn) & mask;
    let knights = board.pieces(Piece::Knight) & mask;
    let bishops = board.pieces(Piece::Bishop) & mask;
    let rooks = board.pieces(Piece::Rook) & mask;
    let queens = board.pieces(Piece::Queen) & mask;
    let kings = board.pieces(Piece::King) & mask;
    let base_score = (pawns.popcnt() * 100
        + knights.popcnt() * 300
        + bishops.popcnt() * 311
        + rooks.popcnt() * 500
        + queens.popcnt() * 900) as i32;
    let piece_table_score = weight_piece_positions(pawns, &TABLE_PAWN, flip)
        + weight_piece_positions(knights, &TABLE_KNIGHT, flip)
        + weight_piece_positions(bishops, &TABLE_BISHOP, flip)
        + weight_piece_positions(rooks, &TABLE_ROOK, flip)
        + weight_piece_positions(queens, &TABLE_QUEEN, flip)
        + weight_piece_positions(kings, &TABLE_KING, flip);
    base_score + piece_table_score
}

fn weight_piece_positions(mut mask: BitBoard, table: &PieceTable, flip: bool) -> i32 {
    if flip {
        mask = mask.reverse_colors();
    }
    let mut acc = 0;
    for el in mask {
        acc += table[7 - el.get_rank().to_index()][el.get_file().to_index()] as i32;
    }
    acc
}

pub fn evaluate(board: Board, aux_state: AuxState) -> Score {
    match board.status() {
        BoardStatus::Stalemate => {
            return Score(0);
        }
        BoardStatus::Checkmate => {
            return Score::very_bad();
        }
        BoardStatus::Ongoing => {}
    }

    let mut score = weight_pieces(
        board,
        *board.color_combined(board.side_to_move()),
        board.side_to_move() != Color::White,
    ) - weight_pieces(
        board,
        *board.color_combined(!board.side_to_move()),
        board.side_to_move() != Color::Black,
    );

    if board.my_castle_rights().has_queenside() {
        score += 40;
    }

    if aux_state.has_castled_ooo[board.side_to_move().to_index()] {
        score += 80;
    }

    Score(score)
}
