use clap::Parser;
use core::fmt::Debug;
use std::collections::BTreeSet;
use std::fmt;
use std::io;
use std::sync::Arc;
use std::sync::Mutex;
use std::thread;
use std::time::Duration;
use std::time::SystemTime;

/// Play a game of chess
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct CliOptions {
    /// Run the game slowly so that a human can follow
    #[arg(long)]
    slow: bool,

    /// Print the board at each round
    #[arg(long)]
    print: bool,

    /// Print the board at each round
    #[arg(long, value_name = "N", default_value_t = 100)]
    rounds: usize,

    /// Play against the machine
    #[arg(long)]
    interactive: bool,
}

fn main() -> () {
    let cli = CliOptions::parse();
    let delay = if cli.slow { Some(700) } else { None };

    if cli.interactive {
        println!("{}", interactive());
    } else {
        println!("{}", play(delay, cli.print, cli.rounds));
    }
}

// -----------------
// Types
// -----------------

#[derive(Debug, PartialEq, Copy, Clone, PartialOrd, Eq, Ord)]
enum Character {
    Hook,
    Knight,
    Bishop,
    King,
    Queen,
    Pawn,
}

#[derive(Debug, PartialEq, Copy, Clone, PartialOrd, Eq, Ord)]
enum Player {
    Black,
    White,
}

#[derive(Debug, PartialEq, Copy, Clone, PartialOrd, Eq, Ord)]
struct Piece(Player, Character);

type Board = [Option<Piece>; 64];

// Pretty printing

struct PrettyBoard(Board);

impl fmt::Display for Character {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let char = match &self {
            Character::Pawn => "♙",
            Character::Hook => "♖",
            Character::Knight => "♘",
            Character::Bishop => "♗",
            Character::Queen => "♕",
            Character::King => "♔",
        };
        write!(f, "{}", char)
    }
}

impl fmt::Display for Piece {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Piece(player, char) = self;
        match player {
            Player::White => write!(f, "\x1b[1m\x1b[94m{char}"),
            Player::Black => write!(f, "\x1b[1m\x1b[31m{char}"),
        }
    }
}

struct Position(Option<Piece>);

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Position(o) = self;
        match o {
            Some(piece) => write!(f, " {piece} "),
            None => write!(f, "   "),
        }
    }
}

impl fmt::Display for PrettyBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let PrettyBoard(board) = self;
        let black = |p: Position, f: &mut fmt::Formatter<'_>| write!(f, "\x1b[40m{p}\x1b[0m");
        let white = |p: Position, f: &mut fmt::Formatter<'_>| write!(f, "\x1b[107m{p}\x1b[0m");
        let at = |i: usize| Position(board[i as usize]);
        let mut row = |n: usize| {
            let from = n * 8;
            let mut r = write!(f, "{} ", n + 1);
            for i in 0..4 {
                r = if row_number(from as u8) % 2 == 0 {
                    r.and(black(at(from + 2 * i), f))
                        .and(white(at(from + 2 * i + 1), f))
                } else {
                    r.and(white(at(from + 2 * i), f))
                        .and(black(at(from + 2 * i + 1), f))
                }
            }
            r.and(write!(f, "\n"))
        };
        row(7)
            .and(row(6))
            .and(row(5))
            .and(row(4))
            .and(row(3))
            .and(row(2))
            .and(row(1))
            .and(row(0))
            .and(write!(f, "   A  B  C  D  E  F  G  H "))
    }
}

// -----------------
// Playing
// -----------------

fn interactive() -> String {
    let mut board = starting_board();
    println!("You are the white player. You begin.");
    println!("{}\n\n", PrettyBoard(board));

    loop {
        let (from, to) = read_user_move(&board);
        board[to as usize] = at(from, &board);
        board[from as usize] = None;

        println!("You played");
        println!("{}\n\n", PrettyBoard(board));
        if checkmate(&board).is_some() {
            return "You win!".to_string();
        }

        thread::sleep(Duration::from_millis(800));
        let history = Arc::new(Mutex::new(BTreeSet::<Board>::new()));
        match make_move(Player::Black, &board, history, 3, true) {
            None => return "Draw!".to_string(),
            Some((b, _)) => board = b,
        }

        println!("I played");
        println!("{}\n\n", PrettyBoard(board));
        if checkmate(&board).is_some() {
            return "You lose!".to_string();
        }
    }
}

fn read_user_move(board: &Board) -> (u8, u8) {
    loop {
        println!("Type the coordinates you want to move from: ");
        let from = read_pos();

        let character = match at(from, board) {
            None => {
                println!("This cell is empty. Choose one with a white piece.");
                continue;
            }
            Some(Piece(Player::Black, _)) => {
                println!("This cell has a black piece. You are playing with the whites.");
                continue;
            }
            Some(Piece(Player::White, c)) => c,
        };

        println!("Type the coordinates you want to move to: ");
        let to = read_pos();

        let possible = piece_moves(Player::White, character, from as usize, board);

        if let None = possible.iter().find(|&&p| p == to) {
            println!("Invalid move. Try again.");
            continue;
        }

        match at(to, board) {
            None => {}
            Some(Piece(Player::Black, _)) => {}
            Some(Piece(Player::White, _)) => {
                println!("You can't take a white piece. Try again.");
                continue;
            }
        };

        return (from, to);
    }
}

fn read_pos() -> u8 {
    loop {
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let pos = input
                    .chars()
                    .nth(0)
                    .and_then(|n| match n {
                        'A' => Some(0),
                        'B' => Some(1),
                        'C' => Some(2),
                        'D' => Some(3),
                        'E' => Some(4),
                        'F' => Some(5),
                        'G' => Some(6),
                        'H' => Some(7),
                        _ => None,
                    })
                    .and_then(|col| {
                        input
                            .chars()
                            .nth(1)
                            .and_then(|n| n.to_digit(10))
                            .map(|row| row - 1) // 1-indexed to 0-indexed
                            .map(|row| row * 8 + col)
                    });
                match pos {
                    None => {}
                    Some(p) => return p as u8,
                }
            }
            Err(_) => {}
        };
        println!("Invalid position. An example valid position is 'A5'");
    }
}

type History = Arc<Mutex<BTreeSet<Board>>>;

fn play(delay: Option<u64>, print: bool, max_rounds: usize) -> String {
    let history = Arc::new(Mutex::new(BTreeSet::new()));
    let mut board = starting_board();
    let mut turn = Player::White;
    println!("{}\n\n", PrettyBoard(board));

    let mut i = 0;
    loop {
        let before = SystemTime::now();
        match make_move(turn, &board, history.clone(), 3, true) {
            None => return "Draw!".to_string(),
            Some((b, _)) => board = b,
        }
        let after = SystemTime::now();
        match delay {
            None => {}
            Some(millis) => {
                let elapsed = after.duration_since(before).expect("delay failure").as_millis();
                if elapsed < millis.into() {
                    thread::sleep(Duration::from_millis(millis - elapsed as u64));
                }
            }
        };
        if print {
            println!("{turn:?} played");
            println!("{}\n\n", PrettyBoard(board));
        }
        i += 1;
        match checkmate(&board) {
            None => {}
            Some(player) => return format!("The winner is: {player:?}"),
        }
        if i >= max_rounds {
            return "Reached max iterations".to_string();
        }
        history.lock().unwrap().insert(board);
        turn = next_player(turn);
    }
}

fn next_player(player: Player) -> Player {
    match player {
        Player::White => Player::Black,
        Player::Black => Player::White,
    }
}

fn checkmate(board: &Board) -> Option<Player> {
    let mut kings = Vec::new();
    for piece in board.iter() {
        match piece {
            Some(Piece(player, Character::King)) => kings.push(player),
            _ => {}
        }
    }
    if let None = kings.iter().find(|&&p| *p == Player::White) {
        return Some(Player::Black);
    }
    if let None = kings.iter().find(|&&p| *p == Player::Black) {
        return Some(Player::White);
    }
    return None;
}

fn best_rating_for(player: Player) -> Rating {
    match player {
        Player::White => 1000,
        Player::Black => -1000,
    }
}

fn make_move(
    player: Player,
    board: &Board,
    history: History,
    look_ahead: u8,
    top_level: bool,
) -> Option<(Board, Rating)> {
    let mut candidates = Vec::new();
    if top_level {
        let mut handles = Vec::new();
        for candidate in player_moves(player, &board) {
            {   let hist = history.lock().unwrap();
                if hist.contains(&candidate) {
                    continue;
                }
            }
            let history = history.clone();
            let handle = thread::spawn(move || -> Rating {
                if checkmate(&candidate).is_some() {
                    return best_rating_for(player);
                }

                if look_ahead == 0 {
                    rate(player, &candidate)
                } else {
                    make_move(
                        next_player(player),
                        &candidate,
                        history,
                        look_ahead - 1,
                        false,
                    )
                    .unwrap()
                    .1
                }
            });
            handles.push((candidate, handle));
        }

        for (candidate, handle) in handles.into_iter() {
            candidates.push((candidate, handle.join().unwrap()));
        }
    } else {
        for candidate in player_moves(player, &board) {
            {
                let hist = history.lock().unwrap();
                if hist.contains(&candidate) {
                    continue;
                }
            }
            if checkmate(&candidate).is_some() {
                candidates.push((candidate, best_rating_for(player)));
                continue;
            }

            if look_ahead == 0 {
                candidates.push((candidate, rate(player, &candidate)));
            } else {
                match make_move(
                    next_player(player),
                    &candidate,
                    history.clone(),
                    look_ahead - 1,
                    false,
                ) {
                    Some((_, r)) => candidates.push((candidate, r)),
                    None => panic!("Oh no"),
                }
            }
        }
    }

    let mut best_board = None;
    let mut all_scores = Vec::new();
    for (candidate, rating) in candidates {
        all_scores.push(rating);
        best_board = match best_board {
            None => Some((candidate, rating)),
            Some((_, best_rating)) => {
                let better = match player {
                    Player::White => rating > best_rating,
                    Player::Black => rating < best_rating,
                };
                if better {
                    Some((candidate, rating))
                } else {
                    best_board
                }
            }
        }
    }
    return best_board;
}

// All possible moves for a Player
fn player_moves(player: Player, board: &Board) -> Vec<Board> {
    let mut results: Vec<Board> = Vec::new();
    for (pos, o_piece) in board.iter().enumerate() {
        let Piece(p, c) = match *o_piece {
            None => continue,
            Some(p) => p,
        };

        // don't move other player's pieces
        if p != player {
            continue;
        }

        // gather all the moves
        let moves = piece_moves(p, c, pos, board);

        // creaete boards with those moves
        for new_pos in moves.iter() {
            // skip moves that take pieces of the same colour as the current player
            if let Some(Piece(other, _)) = at(*new_pos, board) {
                if other == player {
                    continue;
                }
            }
            results.push(board.clone());
            results.last_mut().map(|b| {
                b[pos] = None;
                b[*new_pos as usize] = Some(Piece(p, c));
            });
        }
    }
    results
}

fn piece_moves(player: Player, character: Character, pos: usize, board: &Board) -> Vec<u8> {
    match character {
        Character::Pawn => pawn_moves(player, pos, board),
        Character::Hook => hook_moves(pos, board),
        Character::Bishop => bishop_moves(pos, board),
        Character::Knight => knight_moves(pos),
        Character::Queen => queen_moves(pos, board),
        Character::King => king_moves(pos),
    }
}

fn at(i: u8, board: &Board) -> Option<Piece> {
    board[i as usize]
}

// ----------------------------------------------------------------------
// Piece moves
// ----------------------------------------------------------------------

fn populated(pos: u8, board: &Board) -> bool {
    at(pos as u8, board).is_some()
}

fn pawn_moves(player: Player, pos: usize, board: &Board) -> Vec<u8> {
    let mut moves: Vec<u8> = Vec::new();
    let pos = pos as u8;

    let is_empty = |p| !at(p, board).is_some();

    // move forward
    let fwd_candidates = match player {
        Player::Black => {
            if row_number(pos) == 6 {
                vec![
                    prev_row(pos),
                    prev_row(pos).filter(|p| is_empty(*p)).and_then(prev_row),
                ]
            } else {
                vec![prev_row(pos)]
            }
        }
        Player::White => {
            if row_number(pos) == 1 {
                vec![
                    next_row(pos),
                    next_row(pos).filter(|p| is_empty(*p)).and_then(next_row),
                ]
            } else {
                vec![next_row(pos)]
            }
        }
    };
    let fwd_moves = fwd_candidates.iter().flatten().filter(|pos_| {
        let occupied = at(**pos_, board).is_some();
        !occupied
    });
    for m in fwd_moves {
        moves.push(*m);
    }

    // take piece
    let positions = match player {
        Player::Black => prev_row(pos),
        Player::White => next_row(pos),
    }
    .map_or(Vec::new(), |pos_| vec![next_col(pos_), prev_col(pos_)]);
    for pp in positions.iter() {
        let new_pos = match pp {
            Some(new_pos) => new_pos,
            None => continue,
        };
        if populated(*new_pos, board) {
            moves.push(*new_pos);
        }
    }
    moves
}

fn long_range_moves(pos: usize, board: &Board, fns: Vec<impl Fn(u8) -> Option<u8>>) -> Vec<u8> {
    let mut moves = Vec::new();

    for go in fns {
        let mut new_pos = pos as u8;
        loop {
            new_pos = match go(new_pos) {
                None => break,
                Some(p) => p,
            };
            moves.push(new_pos);
            if populated(new_pos, board) {
                break;
            }
        }
    }
    moves
}

fn bishop_moves(pos: usize, board: &Board) -> Vec<u8> {
    long_range_moves(
        pos,
        board,
        vec![
            |p| next_row(p).and_then(next_col), // north-east
            |p| next_row(p).and_then(prev_col), // north-west
            |p| prev_row(p).and_then(next_col), // south-east
            |p| prev_row(p).and_then(prev_col), // south-west
        ],
    )
}

fn hook_moves(pos: usize, board: &Board) -> Vec<u8> {
    long_range_moves(
        pos,
        board,
        vec![
            |p| next_row(p), // north
            |p| prev_row(p), // south
            |p| next_col(p), // east
            |p| prev_col(p), // west
        ],
    )
}

fn queen_moves(pos: usize, board: &Board) -> Vec<u8> {
    long_range_moves(
        pos,
        board,
        vec![
            |p| next_row(p).and_then(next_col), // north-east
            |p| next_row(p).and_then(prev_col), // north-west
            |p| prev_row(p).and_then(next_col), // south-east
            |p| prev_row(p).and_then(prev_col), // south-west
            |p| next_row(p),                    // north
            |p| prev_row(p),                    // south
            |p| next_col(p),                    // east
            |p| prev_col(p),                    // west
        ],
    )
}

fn short_range_moves(pos: usize, fns: Vec<impl Fn(u8) -> Option<u8>>) -> Vec<u8> {
    let mut moves = Vec::new();

    for go in fns {
        match go(pos as u8) {
            None => {}
            Some(p) => moves.push(p),
        };
    }
    moves
}

fn king_moves(pos: usize) -> Vec<u8> {
    short_range_moves(
        pos,
        vec![
            |p| next_row(p).and_then(next_col), // north-east
            |p| next_row(p).and_then(prev_col), // north-west
            |p| prev_row(p).and_then(next_col), // south-east
            |p| prev_row(p).and_then(prev_col), // south-west
            |p| next_row(p),                    // north
            |p| prev_row(p),                    // south
            |p| next_col(p),                    // east
            |p| prev_col(p),                    // west
        ],
    )
}

fn knight_moves(pos: usize) -> Vec<u8> {
    short_range_moves(
        pos,
        vec![
            |p| next_row(p).and_then(next_row).and_then(next_col), // north-east
            |p| next_row(p).and_then(next_row).and_then(prev_col), // north-west
            |p| prev_row(p).and_then(prev_row).and_then(next_col), // south-east
            |p| prev_row(p).and_then(prev_row).and_then(prev_col), // south-west
            |p| next_col(p).and_then(next_col).and_then(next_row), // east-north
            |p| next_col(p).and_then(next_col).and_then(prev_row), // east-south
            |p| prev_col(p).and_then(prev_col).and_then(next_row), // west-north
            |p| prev_col(p).and_then(prev_col).and_then(prev_row), // west-south
        ],
    )
}

// The rating represents how good the game looks for the white player.
type Rating = (i64, u8);

fn rate(
    _turn: Player, // player that just played
    board: &Board,
) -> Rating {
    let mut w_attack: [bool; 64] = [false; 64]; // positions attacked by white
    let mut b_attack: [bool; 64] = [false; 64]; // positions attacked by black

    let mut attack = |player: Player, pos: u8| {
        match player {
            Player::White => w_attack[pos as usize] = true,
            Player::Black => b_attack[pos as usize] = true,
        };
    };
    for (pos, piece) in board.iter().enumerate() {
        let (player, moves) = match *piece {
            None => continue,
            Some(Piece(player, Character::Pawn)) => (player, pawn_moves(player, pos, board)),
            Some(Piece(player, Character::Hook)) => (player, hook_moves(pos, board)),
            Some(Piece(player, Character::Bishop)) => (player, bishop_moves(pos, board)),
            Some(Piece(player, Character::Knight)) => (player, knight_moves(pos)),
            Some(Piece(player, Character::Queen)) => (player, queen_moves(pos, board)),
            Some(Piece(player, Character::King)) => (player, king_moves(pos)),
        };
        for target in moves.iter() {
            let Piece(other, _) = match at(*target as u8, board) {
                None => continue,
                Some(p) => p,
            };
            if other != player {
                attack(player, *target);
            }
        }
    }

    let mut w_threatened = 0; // white pieces threatened
    let mut w_defended = 0; // white pieces defended (threatened by a white piece)
    let mut b_threatened = 0;
    let mut b_defended = 0;
    let weight = |c| match c {
        Character::Pawn => 1,
        Character::Hook => 5,
        Character::Knight => 3,
        Character::Bishop => 3,
        Character::Queen => 9,
        Character::King => 100,
    };

    let attack_multiplier = 3;
    let defend_multiplier = 1;
    let exists_multiplier = 4;
    let mut w_exists = 0;
    let mut b_exists = 0;

    for (pos, piece) in board.iter().enumerate() {
        match *piece {
            None => {}
            Some(Piece(Player::White, Character::King)) => {
                if b_attack[pos] {
                    w_threatened += weight(Character::King);
                }
            }
            Some(Piece(Player::White, c)) => {
                if b_attack[pos] {
                    w_threatened += attack_multiplier * weight(c);
                    if w_attack[pos] {
                        w_defended += defend_multiplier * weight(c);
                    }
                }
                w_exists += weight(c) * exists_multiplier;
            }
            Some(Piece(Player::Black, Character::King)) => {
                if w_attack[pos] {
                    b_threatened += weight(Character::King);
                }
            }
            Some(Piece(Player::Black, c)) => {
                if w_attack[pos] {
                    b_threatened += attack_multiplier * weight(c);
                    if b_attack[pos] {
                        b_defended += defend_multiplier * weight(c);
                    }
                }
                b_exists += weight(c) * exists_multiplier;
            }
        }
    }
    let white_vulnerable: i64 = w_threatened - w_defended;
    let black_vulnerable: i64 = b_threatened - b_defended;
    return black_vulnerable - white_vulnerable + w_exists - b_exists;
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
