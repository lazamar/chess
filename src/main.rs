use core::mem::size_of;
use std::io;
use core::fmt::Debug;

fn main() -> io::Result<()> {
    let args = std::env::args();
    for argument in args {
        println!("{argument}");
    }

    let board = starting_board();
    rate(&board);
    let player = Player::White;
    let mut best_move = None;
    for candidate in moves(player, &board) {
        let rating = rate(&candidate);
        let best = match best_move.take() {
            Some((best_board, best_rating)) => {
                let better = match player {
                    Player::White => rating > best_rating,
                    Player::Black => rating < best_rating
                };
                if better {
                    (candidate, rating)
                } else {
                    (best_board, best_rating)
                }
            },
            None => (candidate, rating)
        };
        best_move = Some(best);
    }

    println!("{}", size_of::<Piece>());
    println!("{best_move:?}",);
    Ok(())
}

// -----------------
// Types
// -----------------

#[derive(Debug, PartialEq, Copy, Clone)]
enum Character {
    Hook,
    Knight,
    Bishop,
    King,
    Queen,
    Pawn
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum Player {
    Black,
    White
}

#[derive(Debug, PartialEq, Copy, Clone)]
struct Piece(Player, Character);

type Board = [Option<Piece>; 64];


// -----------------
// Moves
// -----------------

fn make_move(player: Player, board: &Board) -> Option<Board> {
  let mut best_board = None;
  let mut best_rating = 0;
  for candidate in moves(player, &board) {
      let rating = rate(&candidate);
      let better = match player {
          Player::White => rating > best_rating,
          Player::Black => rating < best_rating
      };
      if better {
        best_board = Some(candidate);
        best_rating = rating;
      }
  }
  return best_board;
}

struct Moves<'a> {
    board: &'a Board,
    player: Player,
    piece: u8,
    move_: u64,
}

// All possible moves for a Player
fn moves(_player : Player, _board : &Board) -> Vec<Board> {
    Vec::new()
}

// The rating represents how good the game looks for the white player.
type Rating = u64;

fn rate(board : &Board) -> Rating {
    let mut white_attack : [bool; 64] = [false; 64];
    let mut black_attack : [bool; 64] = [false; 64];

    let mut attack = |player: Player, pos: u8| {
        match player {
            Player::White => white_attack[pos as usize] = true,
            Player::Black => black_attack[pos as usize] = true
        };
    };
    for (pos, piece) in board.iter().enumerate() {
        match piece {
            None => {}
            Some(Piece(player, Character::Pawn)) => {
                match &player {
                    Player::White => {
                        next_row(pos as u8).and_then(next_col).map(|p| attack(*player, p));
                        next_row(pos as u8).and_then(prev_col).map(|p| attack(*player, p));
                    }
                    Player::Black => {
                        prev_row(pos as u8).and_then(next_col).map(|p| attack(*player, p));
                        prev_row(pos as u8).and_then(prev_col).map(|p| attack(*player, p));
                    }

                }
            },
            Some(Piece(player, Character::Hook)) => {}
            Some(Piece(player, Character::Knight)) => {}
            Some(Piece(player, Character::Bishop)) => {}
            Some(Piece(player, Character::Queen)) => {}
            Some(Piece(player, Character::King)) => {}
        }
    }
    println!("{white_attack:?}");
    println!("{black_attack:?}");
    0
}

fn next_row(pos: u8) -> Option<u8> {
  let n = pos + 8;
  if  n >= 64 {
      None
  } else {
      Some(n)
  }
}

fn prev_row(pos: u8) -> Option<u8> {
  if pos >= 8 {
      Some(pos - 8)
  } else {
      None
  }
}

fn next_col(pos: u8) -> Option<u8> {
  if pos % 8 == 7 {
      None
  } else {
      Some(pos + 1)
  }
}

fn prev_col(pos: u8) -> Option<u8> {
  if pos % 8 == 0 {
      None
  } else {
      Some(pos - 1)
  }
}


// -----------------
// Tests
// -----------------

#[cfg(test)]
mod chess_tests {
   use super::*;

   #[test]
   fn initial_board() {
       let initial_board = starting_board();
       assert_eq!(initial_board[7], Some(Piece(Player::White, Character::Hook)));
   }

}

fn starting_board() -> Board {
[
  Some(Piece(Player::White, Character::Hook)),
  Some(Piece(Player::White, Character::Knight)),
  Some(Piece(Player::White, Character::Bishop)),
  Some(Piece(Player::White, Character::King)),
  Some(Piece(Player::White, Character::Queen)),
  Some(Piece(Player::White, Character::Bishop)),
  Some(Piece(Player::White, Character::Knight)),
  Some(Piece(Player::White, Character::Hook)),
  Some(Piece(Player::White, Character::Pawn)),
  Some(Piece(Player::White, Character::Pawn)),
  Some(Piece(Player::White, Character::Pawn)),
  Some(Piece(Player::White, Character::Pawn)),
  Some(Piece(Player::White, Character::Pawn)),
  Some(Piece(Player::White, Character::Pawn)),
  Some(Piece(Player::White, Character::Pawn)),
  Some(Piece(Player::White, Character::Pawn)),
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  Some(Piece(Player::Black, Character::Pawn)),
  Some(Piece(Player::Black, Character::Pawn)),
  Some(Piece(Player::Black, Character::Pawn)),
  Some(Piece(Player::Black, Character::Pawn)),
  Some(Piece(Player::Black, Character::Pawn)),
  Some(Piece(Player::Black, Character::Pawn)),
  Some(Piece(Player::Black, Character::Pawn)),
  Some(Piece(Player::Black, Character::Pawn)),
  Some(Piece(Player::Black, Character::Hook)),
  Some(Piece(Player::Black, Character::Knight)),
  Some(Piece(Player::Black, Character::Bishop)),
  Some(Piece(Player::Black, Character::Queen)),
  Some(Piece(Player::Black, Character::King)),
  Some(Piece(Player::Black, Character::Bishop)),
  Some(Piece(Player::Black, Character::Knight)),
  Some(Piece(Player::Black, Character::Hook))]
}
