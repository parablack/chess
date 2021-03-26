use std::str::FromStr;

use chess::{Board, BoardStatus, ChessMove, Game, MoveGen};
use eval::evaluate;
use smallvec::SmallVec;

use crate::eval::{self, AuxState, Score};
pub struct Engine {
    pub game: Game,
    pub aux_state: AuxState,
}

impl Engine {
    pub fn new() -> Self {
        Engine {
            game: Game::new(),
            aux_state: AuxState::new(),
        }
    }

    pub fn set_state_from_moves(&mut self, moves: Vec<ChessMove>) {
        self.game = Game::new();
        for m in moves {
            self.commit(m);
        }
        dbg!(&self.aux_state);
    }

    pub fn commit(&mut self, chess_move: ChessMove) {
        self.aux_state = self
            .aux_state
            .update(&self.game.current_position(), chess_move);
        self.game.make_move(chess_move);
    }

    fn opening_move(board: Board) -> Option<ChessMove> {
        let hardcoded_moves = [
            (
                "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
                "e4",
            ),
            (
                "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1",
                "e5",
            ),
        ];

        for (fen, san) in &hardcoded_moves {
            if board == Board::from_str(fen).unwrap() {
                return Some(ChessMove::from_san(&board, san).unwrap());
            }
        }
        None
    }

    pub fn find_move(&mut self, depth: u8) -> (Score, Option<ChessMove>) {
        let board = self.game.current_position();
        if let Some(m) = Self::opening_move(board) {
            return (Score(0), Some(m));
        }

        debug_assert!(depth <= 8); // SmallVec capacity
        let (score, mut moves) = mini_max(
            depth,
            board,
            self.aux_state.clone(),
            Score::worst(),
            Score::best(),
        );
        (score, moves.pop())
    }
}

fn mini_max(
    depth: u8,
    position: Board,
    aux_state: AuxState,
    alpha: Score,
    beta: Score,
) -> (Score, SmallVec<[ChessMove; 8]>) {
    if depth == 0 || position.status() != BoardStatus::Ongoing {
        return (evaluate(position, aux_state), SmallVec::new());
    }

    let mut max_score = alpha;
    let mut saved = (alpha, SmallVec::new());

    for m in MoveGen::new_legal(&position) {
        let aux_state = aux_state.update(&position, m);
        let is_draw = aux_state.is_draw(); // TODO: This is hacky!
        let (mut score, mut moves) = mini_max(
            depth - 1,
            position.make_move_new(m),
            aux_state,
            beta.flip(),
            max_score.flip(),
        );
        score = score.flip();
        if score.is_decided() {
            score.0 -= score.0.signum(); // prefer fast mates
        }

        moves.push(m);
        if is_draw {
            score.0 -= 666;
        }

        if score > max_score {
            max_score = score;
            saved = (score, moves);
        }

        if max_score >= beta {
            break;
        }
    }

    saved
}
