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

    pub fn find_move(&mut self, depth: u8) -> (Score, Option<ChessMove>) {
        debug_assert!(depth <= 8); // SmallVec capacity
        let (score, mut moves) = mini_max(
            depth,
            self.game.current_position(),
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
