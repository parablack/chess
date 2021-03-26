use std::io::{self, BufRead, Write};
use vampirc_uci::{parse_one, UciInfoAttribute, UciMessage};

mod engine;
mod eval;
mod piece_tables;

use engine::*;
fn main() {
    let stdin = io::stdin();
    let mut engine = Engine::new();
    let mut force = false;

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
            UciMessage::Go { .. } => {
                if let (score, Some(best_move)) = engine.find_move() {
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
                    println!("{}", UciMessage::info_string("Couldn't find move!".into()))
                }
            }
            msg @ _ => {
                dbg!(msg);
                unimplemented!()
            }
        }
        /*
        let line = line.unwrap();
        let words = line.split_whitespace().collect::<Vec<_>>();
        match words[0] {
            "xboard" => println!(),
            "protover" => {
                if words[1] == "2" {
                    println!("feature usermove=1 sigint=0 sigterm=0 time=0 debug=1 done=1");
                }
            }
            "new" => engine = Engine::new(),
            "force" => force = true,
            "go" => force = false,
            "usermove" => {
                let opponent_move = an::parse(words[1]);
                engine.commit(opponent_move);
                if let Some(m) = engine.find_move() {
                    engine.commit(m);
                    println!("move {}", m);
                } else {
                    println!("offer draw");
                }
            }
            "quit" => return,
            "accepted" | "easy" | "post" | "rejected" | "level" => continue,
            _ => println!("# Unknown command: {}", words[0]),
        }
        */
        let _ = std::io::stdout().flush();
    }
}
