use std::io::{self, BufRead, Write};
use vampirc_uci::{parse_one, UciInfoAttribute, UciMessage, UciTimeControl};

mod engine;
mod eval;
mod piece_tables;

use engine::*;
fn main() {
    let stdin = io::stdin();
    let mut engine = Engine::new();

    for line in stdin.lock().lines() {
        let msg: UciMessage = parse_one(&line.unwrap());
        match msg {
            UciMessage::Uci => {
                println!("OOO");
                println!(
                    "{}",
                    UciMessage::Id {
                        name: Some("OOO".into()),
                        author: Some("mflo".into())
                    }
                );
                println!("{}", UciMessage::UciOk)
            }
            UciMessage::Quit => return,
            UciMessage::IsReady => {
                println!("{}", UciMessage::ReadyOk)
            }
            UciMessage::UciNewGame => {}
            UciMessage::Stop => {
                panic!("got stop. oof")
            }
            UciMessage::Position {
                moves, startpos, ..
            } => {
                assert!(startpos);
                engine.set_state_from_moves(moves);
            }
            UciMessage::Go { time_control, .. } => {
                let mut depth = 6;

                if let Some(UciTimeControl::TimeLeft {
                    white_time,
                    black_time,
                    ..
                }) = time_control
                {
                    let own_time = match engine.game.side_to_move() {
                        chess::Color::White => white_time,
                        chess::Color::Black => black_time,
                    };
                    if let Some(own_time) = own_time {
                        match own_time.num_seconds() {
                            0 => depth = 3,
                            1..=3 => depth = 4,
                            4..=7 => depth = 5,
                            _ => depth = 6,
                        }
                    }
                }

                if let (score, Some(best_move)) = engine.find_move(depth) {
                    engine.commit(best_move);
                    println!(
                        "{}",
                        UciMessage::Info(vec![UciInfoAttribute::Score {
                            cp: Some(score.0),
                            mate: None,
                            lower_bound: None,
                            upper_bound: None
                        }]),
                    );
                    println!("{}", UciMessage::best_move(best_move),);
                } else {
                    println!(
                        "{}",
                        UciMessage::info_string("Couldn't find move. This is a bug!".into())
                    )
                }
            }
            msg @ _ => {
                dbg!(msg);
                unimplemented!()
            }
        }
        let _ = std::io::stdout().flush();
    }
}
