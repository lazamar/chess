use core::mem::size_of;
use std::io;
use core::fmt::Debug;

#[derive(Debug)]
enum Character {
    Hook,
    Knight,
    Bishop,
    King,
    Queen,
    Pawn
}

#[derive(Debug)]
enum Colour {
    Black,
    White
}

#[derive(Debug)]
struct Piece(Colour, Character);

type Board = [Option<Piece>; 64];

fn main() -> io::Result<()> {
    let args = std::env::args();
    for argument in args {
        println!("{argument}");
    }

    println!("{}", size_of::<Piece>());
    let initial_board = starting_board();
    println!("{initial_board:?}");
    Ok(())
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
