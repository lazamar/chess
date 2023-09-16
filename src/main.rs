use core::mem::size_of;
use std::io;
use core::fmt::Debug;

fn main() -> io::Result<()> {
    let args = std::env::args();
    for argument in args {
        println!("{argument}");
    }

    let board = starting_board();
    let player = Player::White;
    let mut best_move = None;
    for candidate in moves(player, &board) {
        let rating = rate(&candidate);
        best_move = Some(match best_move.take() {
            Some((best_board, best_rating)) => {
                if rating > best_rating {
                    (candidate, rating)
                } else {
                    (best_board, best_rating)
                }
            },
            None => (candidate, rating)
        });
    }

    println!("{}", size_of::<Piece>());
    println!("{best_move:?}",);
    Ok(())
}

// -----------------
// Types
// -----------------

#[derive(Debug, PartialEq)]
enum Character {
    Hook,
    Knight,
    Bishop,
    King,
    Queen,
    Pawn
}

#[derive(Debug, PartialEq)]
enum Player {
    Black,
    White
}

#[derive(Debug, PartialEq)]
struct Piece(Player, Character);

type Board = [Option<Piece>; 64];


// -----------------
// Moves
// -----------------

struct Moves<'a> {
    board: &'a Board,
    player: Player,
    piece: u8,
    move_: u64,
}

// All possible moves for a Player
fn moves(player : Player, board : &Board) -> Vec<Board> {
    Vec::new()
}

type Rating = u64;

fn rate(board : &Board) -> Rating {
    0
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
  Some(Piece(Player::White, Character::Pawn)),
  Some(Piece(Player::White, Character::Pawn)),
  Some(Piece(Player::White, Character::Pawn)),
  Some(Piece(Player::White, Character::Pawn)),
  Some(Piece(Player::White, Character::Pawn)),
  Some(Piece(Player::White, Character::Pawn)),
  Some(Piece(Player::White, Character::Pawn)),
  Some(Piece(Player::White, Character::Pawn)),
  Some(Piece(Player::White, Character::Hook)),
  Some(Piece(Player::White, Character::Knight)),
  Some(Piece(Player::White, Character::Bishop)),
  Some(Piece(Player::White, Character::Queen)),
  Some(Piece(Player::White, Character::King)),
  Some(Piece(Player::White, Character::Bishop)),
  Some(Piece(Player::White, Character::Knight)),
  Some(Piece(Player::White, Character::Hook))]
}
