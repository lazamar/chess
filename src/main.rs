use core::mem::size_of;
use std::io;
use core::fmt::Debug;

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
enum Colour {
    Black,
    White
}

#[derive(Debug, PartialEq)]
struct Piece(Colour, Character);

type Board = [Option<Piece>; 64];

fn main() -> io::Result<()> {
    let args = std::env::args();
    for argument in args {
        println!("{argument}");
    }

    println!("{}", size_of::<Piece>());
    Ok(())
}

#[cfg(test)]
mod chess_tests {
   use super::*;

   #[test]
   fn initial_board() {
       let initial_board = starting_board();
       assert_eq!(initial_board[7], Some(Piece(Colour::White, Character::Hook)));
   }

  fn starting_board() -> Board {
    [
      Some(Piece(Colour::White, Character::Hook)),
      Some(Piece(Colour::White, Character::Knight)),
      Some(Piece(Colour::White, Character::Bishop)),
      Some(Piece(Colour::White, Character::King)),
      Some(Piece(Colour::White, Character::Queen)),
      Some(Piece(Colour::White, Character::Bishop)),
      Some(Piece(Colour::White, Character::Knight)),
      Some(Piece(Colour::White, Character::Hook)),
      Some(Piece(Colour::White, Character::Pawn)),
      Some(Piece(Colour::White, Character::Pawn)),
      Some(Piece(Colour::White, Character::Pawn)),
      Some(Piece(Colour::White, Character::Pawn)),
      Some(Piece(Colour::White, Character::Pawn)),
      Some(Piece(Colour::White, Character::Pawn)),
      Some(Piece(Colour::White, Character::Pawn)),
      Some(Piece(Colour::White, Character::Pawn)),
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
      Some(Piece(Colour::White, Character::Pawn)),
      Some(Piece(Colour::White, Character::Pawn)),
      Some(Piece(Colour::White, Character::Pawn)),
      Some(Piece(Colour::White, Character::Pawn)),
      Some(Piece(Colour::White, Character::Pawn)),
      Some(Piece(Colour::White, Character::Pawn)),
      Some(Piece(Colour::White, Character::Pawn)),
      Some(Piece(Colour::White, Character::Pawn)),
      Some(Piece(Colour::White, Character::Hook)),
      Some(Piece(Colour::White, Character::Knight)),
      Some(Piece(Colour::White, Character::Bishop)),
      Some(Piece(Colour::White, Character::Queen)),
      Some(Piece(Colour::White, Character::King)),
      Some(Piece(Colour::White, Character::Bishop)),
      Some(Piece(Colour::White, Character::Knight)),
      Some(Piece(Colour::White, Character::Hook))]
  }
}
