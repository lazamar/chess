use core::fmt::Debug;
use std::fmt;

fn main() -> () {
    println!("{}", play());
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
    Pawn,
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum Player {
    Black,
    White,
}

#[derive(Debug, PartialEq, Copy, Clone)]
struct Piece(Player, Character);

type Board = [Option<Piece>; 64];

struct PrettyBoard(Board);

impl fmt::Display for Character {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let char = match &self {
            Character::Pawn => "♙",
            Character::Hook => "♖",
            Character::Knight => "♘",
            Character::Bishop => "♗",
            Character::Queen => "♕",
            Character::King => "♔"
        };
        write!(f, "{}", char)
    }
}

impl fmt::Display for Piece {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Piece(player,char) = self;
        match player {
            Player::White => write!(f, "\x1b[32m{char}"),
            Player::Black => write!(f, "\x1b[31m{char}")
        }
    }
}

struct Position(Option<Piece>);

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Position(o) = self;
        match o {
            Some(piece) => write!(f, " {piece} "),
            None => write!(f, "   ")
        }
    }
}

impl fmt::Display for PrettyBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let PrettyBoard(board) = self;
        let black = |p : Position, f: &mut fmt::Formatter<'_>| write!(f, "\x1b[40m{p}\x1b[0m");
        let white = |p : Position, f: &mut fmt::Formatter<'_>| write!(f, "\x1b[107m{p}\x1b[0m");
        let at = |i : usize| Position(board[i as usize]);
        let mut row = |from : usize| {
            let mut r = Ok(());
            for i in 0..3 {
                r = if row_number(from as u8) % 2 == 0 {
                    r.and(black(at(from + 2*i), f)).and(white(at(from + 2*i + 1), f))
                } else {
                    r.and(white(at(from + 2*i), f)).and(black(at(from + 2*i + 1), f))
                }
            }
            r.and(write!(f, "\n"))
        };
        row(0 * 8)
            .and(row(1 * 8))
            .and(row(2 * 8))
            .and(row(3 * 8))
            .and(row(4 * 8))
            .and(row(5 * 8))
            .and(row(6 * 8))
            .and(row(7 * 8))
    }
}

// -----------------
// Playing
// -----------------

fn play() -> String {
    let mut board = starting_board();
    let mut turn = Player::White;
    let next = |p| match p {
        Player::White => Player::Black,
        Player::Black => Player::White
    };

    let max_rounds = 8;
    let mut i = 0;
    loop {
        turn = next(turn);
        match make_move(turn, &board) {
            None => return "Draw!".to_string(),
            Some(b) => match checkmate(&b) {
                None => board = b,
                Some(player) => return format!("The origin is: {player:?}")
            }
        }
        println!("{}", PrettyBoard(board));
        i += 1;
        if i > max_rounds { return "Reached max iterations".to_string(); }
    }
}

//TODO
fn checkmate(_: &Board) -> Option<Player> {
    return None
}

fn make_move(player: Player, board: &Board) -> Option<Board> {
    let mut best_board = None;
    let mut best_rating = 0;
    for candidate in moves(player, &board) {
        let rating = rate(&candidate);
        let better = match player {
            Player::White => rating > best_rating,
            Player::Black => rating < best_rating,
        };
        if better || best_rating == 0 {
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
fn moves(_player: Player, board: &Board) -> Vec<Board> {
    let mut v = Vec::new();
    v.push(board.clone());
    v
}

// The rating represents how good the game looks for the white player.
type Rating = u64;

fn rate(board: &Board) -> Rating {
    let mut w_attack: [bool; 64] = [false; 64];
    let mut b_attack: [bool; 64] = [false; 64];

    let mut attack = |player: Player, pos: u8| {
        match player {
            Player::White => w_attack[pos as usize] = true,
            Player::Black => b_attack[pos as usize] = true,
        };
    };
    for (pos, piece) in board.iter().enumerate() {
        match piece {
            None => {}
            Some(Piece(player, Character::Pawn)) => match &player {
                Player::White => {
                    next_row(pos as u8)
                        .and_then(next_col)
                        .map(|p| attack(*player, p));
                    next_row(pos as u8)
                        .and_then(prev_col)
                        .map(|p| attack(*player, p));
                }
                Player::Black => {
                    prev_row(pos as u8)
                        .and_then(next_col)
                        .map(|p| attack(*player, p));
                    prev_row(pos as u8)
                        .and_then(prev_col)
                        .map(|p| attack(*player, p));
                }
            },
            Some(Piece(player, Character::Hook)) => {}
            Some(Piece(player, Character::Knight)) => {}
            Some(Piece(player, Character::Bishop)) => {}
            Some(Piece(player, Character::Queen)) => {}
            Some(Piece(player, Character::King)) => {}
        }
    }

    let mut w_threatened = 0;
    let mut w_defended = 0;
    let mut b_threatened = 0;
    let mut b_defended = 0;
    for (pos, piece) in board.iter().enumerate() {
        match piece {
            None => {}
            Some(Piece(Player::White, _)) => {
                if b_attack[pos] {
                    w_threatened += 1;
                }
                if w_attack[pos] {
                    w_defended += 1;
                }
            }
            Some(Piece(Player::Black, _)) => {
                if b_attack[pos] {
                    b_defended += 1;
                }
                if w_attack[pos] {
                    b_threatened += 1;
                }
            }
        }
    }
    let white_vulnerable = w_threatened - w_defended;
    let black_vulnerable = b_threatened - b_defended;
    return black_vulnerable - white_vulnerable;
}

fn next_row(pos: u8) -> Option<u8> {
    let n = pos + 8;
    if n >= 64 {
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

fn row_number(pos: u8) -> u8 {
    pos / 8
}

fn col_number(pos: u8) -> u8 {
    pos % 8
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
        assert_eq!(
            initial_board[7],
            Some(Piece(Player::White, Character::Hook))
        );
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
        Some(Piece(Player::Black, Character::Hook)),
    ]
}
