use chess::{Board, ChessMove, Game, MoveGen};
use eval::evaluate;
use smallvec::SmallVec;

use crate::eval::{self, AuxState, Score};
pub struct Engine {
    game: Game,
    aux_state: AuxState,
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

    pub fn find_move(&mut self) -> (Score, Option<ChessMove>) {
        // MoveGen::new_legal(&self.game.current_position()).next()
        let (score, mut moves) =
            simple_min_max(5, self.game.current_position(), self.aux_state.clone());
        (score, moves.pop())
    }
}

fn simple_min_max(
    depth: u8,
    position: Board,
    aux_state: AuxState,
) -> (Score, SmallVec<[ChessMove; 8]>) {
    if depth == 0 {
        return (evaluate(position, aux_state), SmallVec::new());
    }

    MoveGen::new_legal(&position)
        .into_iter()
        .map(|m| {
            let aux_state = aux_state.update(&position, m);
            let is_draw = aux_state.is_draw(); // TODO: This is hacky!
            let (mut score, mut moves) =
                simple_min_max(depth - 1, position.make_move_new(m), aux_state);
            moves.push(m);
            if is_draw {
                score.0 += 200;
            }
            (score.flip(), moves)
        })
        .max_by_key(|(score, _)| *score)
        .unwrap_or_else(|| (Score::worst(), SmallVec::new()))
}
