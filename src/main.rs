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
    #[arg(long, value_name = "N", default_value_t = 500)]
    rounds: usize,

    /// Play against the machine
    #[arg(long)]
    interactive: bool,

    // Enable debugging logging
    #[arg(long)]
    debug: bool,

    // How many moves to look ahead
    #[arg(long, value_name = "N", default_value_t = 5)]
    foresight: u8,
}

fn main() -> () {
    let cli = CliOptions::parse();
    let delay = if cli.slow { Some(1000) } else { None };

    if cli.interactive {
        println!("{}", interactive());
    } else {
        println!("{}", play(delay, cli.print, cli.rounds, cli.debug, LookAhead(cli.foresight)));
    }
}

// -----------------
// Types
// -----------------

#[derive(Debug, PartialEq, Copy, Clone, PartialOrd, Eq, Ord)]
pub enum Character {
    Rook,
    Knight,
    Bishop,
    King,
    Queen,
    Pawn,
}

#[derive(Debug, PartialEq, Copy, Clone, PartialOrd, Eq, Ord)]
pub enum Player {
    Black,
    White,
}

#[derive(Debug, PartialEq, Copy, Clone, PartialOrd, Eq, Ord)]
pub struct Piece(Player, Character);

type Board = [Option<Piece>; 64];

// Pretty printing

struct PrettyBoard {
    board: Board,
    debug: bool,
    moved: Option<u8>,
}

impl fmt::Display for Character {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let char = match &self {
            Character::Pawn => "♙",
            Character::Rook => "♖",
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
        let PrettyBoard { board, debug, moved } = *self;
        let was_moved = |ix| Some(ix as u8) == moved;
        let black = |p: Position, highlight: bool, f: &mut fmt::Formatter<'_>| {
            if highlight {
                write!(f, "\x1b[45m{p}\x1b[0m")
            } else {
                write!(f, "\x1b[40m{p}\x1b[0m")
            }
        };
        let white = |p: Position, highlight: bool, f: &mut fmt::Formatter<'_>| {
            if highlight {
                write!(f, "\x1b[45m{p}\x1b[0m")
            } else {
                write!(f, "\x1b[107m{p}\x1b[0m")
            }
        };
        let at = |i: usize| Position(board[i as usize]);
        let mut row = |n: usize| {
            let from = n * 8;
            let mut r = write!(f, "{} ", n + 1);
            for i in 0..4 {
                let ix = from + 2 * i;
                r = if row_number(from as u8) % 2 == 0 {
                    r.and(black(at(ix), was_moved(ix), f))
                        .and(white(at(ix + 1), was_moved(ix + 1), f))
                } else {
                    r.and(white(at(ix), was_moved(ix), f))
                        .and(black(at(ix + 1), was_moved(ix + 1), f))
                }
            }

            if debug {
                write!(f, " \"")?;
                for i in from..(from + 8) {
                    let char = match at(i).0 {
                        None => ' ',
                        Some(Piece(Player::White, Character::Rook)) => 'r',
                        Some(Piece(Player::White, Character::Knight)) => 'h',
                        Some(Piece(Player::White, Character::Bishop)) => 'b',
                        Some(Piece(Player::White, Character::Queen)) => 'q',
                        Some(Piece(Player::White, Character::King)) => 'k',
                        Some(Piece(Player::White, Character::Pawn)) => 'p',
                        Some(Piece(Player::Black, Character::Rook)) => 'R',
                        Some(Piece(Player::Black, Character::Knight)) => 'H',
                        Some(Piece(Player::Black, Character::Bishop)) => 'B',
                        Some(Piece(Player::Black, Character::Queen)) => 'Q',
                        Some(Piece(Player::Black, Character::King)) => 'K',
                        Some(Piece(Player::Black, Character::Pawn)) => 'P',
                    };
                    write!(f, "{}", char)?;
                }
                write!(f, "\"")?;
            }
            write!(f, "\n")
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

fn diff(from: &Board, to: &Board, debug: bool) -> PrettyBoard {
    let mut moved : Option<u8> = None;
    for i in 0..64 {
        if from[i] != to[i] && to[i].is_some() {
            moved = Some(i as u8);
        }
    }
    PrettyBoard{ board: *to, debug, moved }
}

// -----------------
// Playing
// -----------------

const DEFAULT_LOOKAHEAD : LookAhead = LookAhead(4);

fn interactive() -> String {
    let mut board = starting_board();
    println!("You are the white player. You begin.");
    println!(
        "{}\n\n",
        PrettyBoard {
            board,
            debug: false,
            moved: None
        }
    );

    loop {
        let (from, to) = read_user_move(&board);
        board[to as usize] = at(from, &board);
        board[from as usize] = None;

        println!("You played");
        println!(
            "{}\n\n",
            PrettyBoard {
                board,
                debug: false,
                moved: None
            }
        );

        thread::sleep(Duration::from_millis(800));
        let history = empty_history();
        match make_move(
            Player::Black,
            &board,
            history,
            DEFAULT_LOOKAHEAD) {
            None => return "You win!".to_string(),
            Some((b, _)) => board = *b,
        }

        println!("I played");
        println!(
            "{}\n\n",
            PrettyBoard {
                board,
                debug: false,
                moved: None
            }
        );
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
            Ok(_) => match to_pos(input) {
                None => {}
                Some(pos) => return pos as u8,
            },
            Err(_) => {}
        };
        println!("Invalid position. An example valid position is 'A5'");
    }
}

fn to_pos(str: String) -> Option<usize> {
    str.chars()
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
            str.chars()
                .nth(1)
                .and_then(|n| n.to_digit(10))
                .map(|row| row - 1) // 1-indexed to 0-indexed
                .map(|row| row * 8 + col)
        })
        .map(|p| p as usize)
}

type History = Arc<Mutex<BTreeSet<Board>>>;

fn empty_history() -> History {
    Arc::new(Mutex::new(BTreeSet::<Board>::new()))
}

fn play(delay: Option<u64>, print: bool, max_rounds: usize, debug: bool, foreseight: LookAhead) -> String {
    let history = Arc::new(Mutex::new(BTreeSet::new()));
    let mut board = starting_board();
    let mut prev = board.clone();
    let mut turn = Player::White;
    println!("{}\n\n", PrettyBoard { board, debug, moved: None });

    let mut i = 0;
    loop {
        let before = SystemTime::now();
        match make_move(turn, &board, history.clone(), foreseight) {
            None => return format!("The winner is: {turn:?}").to_string(),
            Some((b, _)) => board = *b,
        }
        let after = SystemTime::now();
        match delay {
            None => {}
            Some(millis) => {
                let elapsed = after
                    .duration_since(before)
                    .expect("delay failure")
                    .as_millis();
                if elapsed < millis.into() {
                    thread::sleep(Duration::from_millis(millis - elapsed as u64));
                }
            }
        };
        if print {
            println!("{turn:?} played");
            println!("{}\n\n",  diff(&prev, &board, debug))
        }
        prev = board.clone();
        i += 1;
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

fn best_rating_for(player: Player) -> Rating {
    match player {
        Player::White => 1000,
        Player::Black => -1000,
    }
}

#[derive(Debug, Copy, Clone)]
struct LookAhead(u8);

fn make_move(
    player: Player,
    board: &Board,
    history: History,
    look_ahead: LookAhead,
) -> Option<(Arc<Board>, Rating)> {
    let parallelise = look_ahead.0 > 2;
    make_move_(player, board, history, look_ahead, parallelise)
}

fn make_move_(
    player: Player,
    board: &Board,
    history: History,
    look_ahead: LookAhead,
    parallelise: bool,
) -> Option<(Arc<Board>, Rating)> {
    let mut candidates : Vec<(Arc<Board>, Rating)> = Vec::new();
    let is_last_iteration = look_ahead.0 == 0;
    if parallelise {
        let mut handles = Vec::new();
        for i in 0..12 {
            let history = history.clone();
            let board = board.clone();
            let handle = thread::spawn(move || -> Vec<(Arc<Board>,Rating)> {
                let mut rs = Vec::new();
                let moves : Vec<Arc<Board>> = player_moves(player, &board)
                    .into_iter()
                    .enumerate()
                    .filter(|(ix,_)| *ix == i)
                    .map(|(_,c)| c)
                    .collect();
                for candidate in prune(player, moves) {
                    {
                        let hist = history.lock().unwrap();
                        if hist.contains(candidate.as_ref()) { continue; }
                    }

                    let rating = if is_last_iteration {
                        match rate::rate_board(player, &candidate) {
                            None => continue,
                            Some(r) => r
                        }
                    } else {
                        make_move_(
                            next_player(player),
                            &candidate,
                            history.clone(),
                            LookAhead(look_ahead.0 - 1),
                            false,
                        )
                        .unwrap()
                        .1
                    };
                    rs.push((candidate.clone(), rating));
                }
                return rs;
            });
            handles.push(handle);
        }

        for handle in handles.into_iter() {
            for c in handle.join().unwrap().iter() {
                candidates.push(c.clone());
            }
        }
    } else {
        let moves = player_moves(player, &board);
        for candidate in prune(player, moves) {
            if history.lock().unwrap().contains(candidate.as_ref()) {
                continue;
            }

            if is_last_iteration {
                if let Some(r) = rate::rate_board(player, &candidate) {
                    candidates.push((candidate.clone(), r));
                };
            } else {
                let rating = match make_move_(
                    next_player(player),
                    &candidate,
                    history.clone(),
                    LookAhead(look_ahead.0 - 1),
                    false,
                ) {
                    None => continue,
                    Some(r) => r.1
                };
                candidates.push((candidate, rating));
            }
        }
    }

    let mut best_board = None;
    for (candidate, rating) in candidates {
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

// Pick best moves for a player
fn prune(player : Player, bs : Vec<Arc<Board>>) -> Vec<Arc<Board>> {
    let pruned_count = 10;
    let mut best: Vec<(Rating, Arc<Board>)> = bs
        .clone()
        .iter()
        .map(|c| rate::rate_board(player, c))
        .flatten()
        .zip(bs)
        .collect();
    best.sort_by_key(|x| x.0);
    match player {
        Player::White => best.reverse(),
        Player::Black => {},
    };
    return best.into_iter().take(pruned_count).map(|c| c.1).collect();
}

// All possible moves for a Player
fn player_moves(player: Player, board: &Board) -> Vec<Arc<Board>> {
    let mut results: Vec<Arc<Board>> = Vec::new();
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

        // create boards with those moves
        for new_pos in moves.iter() {
            // skip moves that take pieces of the same colour as the current player
            if let Some(Piece(other, _)) = at(*new_pos, board) {
                if other == player {
                    continue;
                }
            }

            let mut b = board.clone();
            b[pos] = None;
            b[*new_pos as usize] = Some(Piece(p, c));

            results.push(Arc::new(b));
        }
    }
    results
}

fn piece_moves(player: Player, character: Character, pos: usize, board: &Board) -> Vec<u8> {
    match character {
        Character::Pawn => pawn_moves(player, pos, board),
        Character::Rook => rook_moves(pos, board),
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

fn rook_moves(pos: usize, board: &Board) -> Vec<u8> {
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

use rate::Rating;
mod rate {
    use super::*;

    // The rating represents how good the game looks for the white player.
    pub type Rating = i64;

    pub fn rate_board(
        turn: Player, // player that just played
        board: &Board,
    ) -> Option<Rating> {
        let attacks = attack_surface(board);
        // skip illegal moves
        if rate::is_in_check(turn, board, &attacks) {
            return None;
        }
        Some(piece_weights(board))
    }

    fn weight(c: Character) -> i64 {
        match c {
            Character::Pawn => 1,
            Character::Rook => 5,
            Character::Knight => 3,
            Character::Bishop => 3,
            Character::Queen => 9,
            Character::King => 100,
        }
    }

    // Sum of weights of white pieces minus black ones.
    fn piece_weights(board: &Board) -> i64 {
        let mut total = 0;
        for piece in board {
            match *piece {
                None => continue,
                Some(Piece(Player::Black, c)) => total -= weight(c),
                Some(Piece(Player::White, c)) => total += weight(c),
            };
        }
        return total;
    }

    fn attack_opportunities(
        turn: Player, // player that just played
        board: &Board,
    ) -> Option<Rating> {
        let attacked_by = attack_surface(board);
        let attack_multiplier = |p| if p == turn { 2 } else { 1 };
        let defend_multiplier = 1;

        let mut w_threatened = 0; // white pieces threatened
        let mut w_defended = 0; // white pieces defended (threatened by a white piece)
        let mut b_threatened = 0;
        let mut b_defended = 0;
        for (pos, piece) in board.iter().enumerate() {
            match *piece {
                None => {}
                Some(Piece(Player::White, Character::King)) => {
                    if attacked_by.black[pos] {
                        if turn == Player::White { return None }
                        w_threatened += weight(Character::King);
                    }
                }
                Some(Piece(Player::White, c)) => {
                    if attacked_by.black[pos] {
                        w_threatened += attack_multiplier(Player::White) * weight(c);
                        if attacked_by.white[pos] {
                            w_defended += defend_multiplier * weight(c);
                        }
                    }
                }
                Some(Piece(Player::Black, Character::King)) => {
                    if attacked_by.white[pos] {
                        if turn == Player::Black { return None }
                        b_threatened += weight(Character::King);
                    }
                }
                Some(Piece(Player::Black, c)) => {
                    if attacked_by.white[pos] {
                        b_threatened += attack_multiplier(Player::Black) * weight(c);
                        if attacked_by.black[pos] {
                            b_defended += defend_multiplier * weight(c);
                        }
                    }
                }
            }
        }
        let white_vulnerable = w_threatened - w_defended;
        let black_vulnerable = b_threatened - b_defended;
        return Some(black_vulnerable - white_vulnerable);
    }

    pub fn checkmated(
        player:Player, // check if this player is checkmated
        history: History,
        board: &Board) -> bool {
        !make_move(
            player,
            board,
            history,
            LookAhead(0))
            .is_some()
    }

    pub fn is_in_check(
        player: Player,
        board: &Board,
        attacked_by: &AttackSurface) -> bool {
        for (i, piece) in board.iter().enumerate() {
            match (player, piece) {
                (Player::White, Some(Piece(Player::White, Character::King))) =>
                    return attacked_by.black[i],
                (Player::Black, Some(Piece(Player::Black, Character::King))) =>
                    return attacked_by.white[i],
                _ => continue
            };
        }
        return true;
    }


    #[derive(Debug)]
    pub struct AttackSurface {
        pub white: [bool; 64], // positions attacked by white
        pub black: [bool; 64], // positions attacked by black
        pub white_count: i64,
        pub black_count: i64,
    }

    pub fn attack_surface(board : &Board) -> AttackSurface {
        let mut attacked_by = AttackSurface {
            white: [false; 64],
            black: [false; 64],
            white_count: 0,
            black_count: 0
        };
        for (pos, piece ) in board.iter().enumerate() {
            let (player, moves) = match piece {
                None => continue,
                Some(Piece(player, character)) => (*player, piece_moves(*player, *character, pos, board))
            };
            for target in moves.iter() {
                match player {
                    Player::White => {
                        attacked_by.white_count += 1;
                        attacked_by.white[*target as usize] = true;
                    },
                    Player::Black => {
                        attacked_by.black_count += 1;
                        attacked_by.black[*target as usize] = true;
                    }
                }
            }
        }
        return attacked_by;
    }
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

#[rustfmt::skip]
fn starting_board() -> Board {
    make_board(
        [ "RHBQKBHR"
        , "PPPPPPPP"
        , "        "
        , "        "
        , "        "
        , "        "
        , "pppppppp"
        , "rhbkqbhr" ])
}

// -----------------
// Tests
// -----------------

fn make_board(mut chars: [&str; 8]) -> Board {
    let mut board: Board = [None; 64];
    let mut ix = 0;
    chars.reverse();
    for row in chars {
        for e in row.chars() {
            board[ix] = match e {
                ' ' => None,
                'r' => Some(Piece(Player::White, Character::Rook)),
                'h' => Some(Piece(Player::White, Character::Knight)),
                'b' => Some(Piece(Player::White, Character::Bishop)),
                'q' => Some(Piece(Player::White, Character::Queen)),
                'k' => Some(Piece(Player::White, Character::King)),
                'p' => Some(Piece(Player::White, Character::Pawn)),
                'R' => Some(Piece(Player::Black, Character::Rook)),
                'H' => Some(Piece(Player::Black, Character::Knight)),
                'B' => Some(Piece(Player::Black, Character::Bishop)),
                'Q' => Some(Piece(Player::Black, Character::Queen)),
                'K' => Some(Piece(Player::Black, Character::King)),
                'P' => Some(Piece(Player::Black, Character::Pawn)),
                _ => panic!("unexpected char"),
            };
            ix += 1;
        }
    }
    return board;
}

struct Rationale(Vec<Option<(Arc<Board>, Rating)>>);

impl fmt::Display for Rationale {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut prev: Arc<Board> = match self.0.first() {
            None => return write!(f, "<NO_MOVES>"),
            Some(None) => return write!(f, "<NO_OPTION>"),
            Some(Some((board,_))) => board.clone(),
        };
        for b in self.0.iter() {
            _ = match b {
                None => write!(f, "<NO_OPTION>"),
                Some((board, rating)) => {
                    let d = diff(&prev, &board, false);
                    prev = board.clone();
                    writeln!(f, "Rating: {}\n{}", rating, d)
                }
            }
        }
        return write!(f, "");
    }
}

// Show all steps considered for the next move.
fn rationalise(
    player: Player,
    board: &Board,
    history: History,
    LookAhead(n): LookAhead) -> Rationale {
    let mut rationale = Vec::new();
    rationale.push(Some((Arc::new(*board), 9999)));
    let mut turn = player;
    let mut next = Arc::new(*board);
    for i in 0..(n + 1) {
        let new_board = make_move(
            player,
            &next,
            history.clone(),
            LookAhead(n - i));
        rationale.push(new_board.clone());
        match new_board {
            None => break,
            Some((b,_)) => next = b
        }
        turn = next_player(turn);
    }
    return Rationale(rationale);
}

#[rustfmt::skip]
#[cfg(test)]
mod chess_tests {
    use super::*;

    #[test]
    fn detect_checkmate() {
        let board = make_board(
            [ "        "
            , "        "
            , "        "
            , "        "
            , "        "
            , "        "
            , "q       "
            , "q    K  "]);
        assert_eq!(
            true,
            rate::checkmated(Player::Black, empty_history(), &board));
    }

    #[test]
    fn play_checkmate() {
        let mut board = make_board(
            [ "RHBQKBHR"
            , "   q    "
            , "        "
            , "        "
            , "        "
            , "        "
            , "        "
            , "  k     "]);

        board = *make_move(
            Player::White,
            &board,
            empty_history(),
            LookAhead(0))
            .unwrap().0;

        println!("{}", PrettyBoard { board, debug: false, moved: None });
        assert_eq!(
            true,
            rate::checkmated(Player::Black, empty_history(), &board));
    }

    // TODO
    #[ignore]
    #[test]
    fn dont_take_equivalent_swap() {
        let mut board = make_board(
            [ "R  QKBHR"
            , "PPPH PPP"
            , "  B     "
            , "   PP   "
            , "    pp  "
            , "bph     "
            , "p pp  pp"
            , "r k qbhr"]);

        board = *make_move(
            Player::White,
            &board,
            empty_history(),
            DEFAULT_LOOKAHEAD)
            .unwrap().0;
        let bishop_count = board.iter().filter(|p| is(Character::Bishop, **p)).count();
        assert_eq!(bishop_count, 4);
    }

    fn is(character: Character, piece: Option<Piece>) -> bool {
        match piece {
            None => false,
            Some(Piece(_,c)) => c == character
        }
    }

    fn at_pos(str: &'static str, board: &Board) -> Option<Piece> {
        at(to_pos(str.to_string()).unwrap() as u8, board)
    }

    #[test]
    fn dont_give_pawn_away() {
        let mut board = make_board(
            [ "RHBQKBHR"
            , "PPP  PPP"
            , "   P    "
            , "    P   "
            , "        "
            , " p      "
            , "pbpppppp"
            , "rh kqbhr"]);
        board = *make_move(
            Player::White,
            &board,
            empty_history(),
            DEFAULT_LOOKAHEAD
        ).unwrap().0;
        assert_eq!(at_pos("F4", &board), None);
    }

    // TODO
    #[ignore]
    #[test]
    fn take_the_pawn() {
        let prev = make_board(
            [ "RHBQKBHR"
            , " PPPPPPP"
            , "        "
            , "        "
            , "P       "
            , "  h  h  "
            , "pppppppp"
            , " rbkqb r" ]);
        let board = *make_move(
            Player::White,
            &prev,
            empty_history(),
            DEFAULT_LOOKAHEAD
        ).unwrap().0;
        println!("{}", diff(&prev, &board, false));
        assert_eq!(at_pos("A4", &board), Some(Piece(Player::Black, Character::Pawn)));
    }

    #[test]
    fn move_the_knight() {
        let prev = make_board(
            [ "RHBQKBHR"
            , "  P PPPP"
            , " P      "
            , "P       "
            , "   P    "
            , "  h     "
            , "pppppppp"
            , "r bkqbhr" ]);
        let board = *make_move(
            Player::White,
            &prev,
            empty_history(),
            DEFAULT_LOOKAHEAD
        ).unwrap().0;
        println!("{}", rationalise(
            Player::White,
            &prev,
            empty_history(),
            DEFAULT_LOOKAHEAD));
        assert_eq!(at_pos("C3", &board), None);
    }

    #[test]
    fn check_detection() {
        let board = make_board(
            [ "RHB K HR"
            , "    P  P"
            , " PP  P B"
            , "        "
            , "P  Q  p "
            , "     p  "
            , "ppp    q"
            , "rh k bhr"]);
        println!("{}", PrettyBoard {
            board: board,
            debug:false,
            moved: None });
        assert_eq!(true, rate::is_in_check(
            Player::White,
            &board,
            &rate::attack_surface(&board)));
    }

    #[test]
    fn attack_queen() {
        let board = make_board(
            [ "        "
            , "        "
            , "P       "
            , "P       "
            , "   P    "
            , "  P     "
            , "        "
            , "q     PP"]);
        let attacked_by = rate::attack_surface(&board);

        println!("{}", PrettyBoard {
            board: board,
            debug:false,
            moved: None });

        assert_eq!(true, attacked_by.white[to_pos("A5".to_string()).unwrap()]);
        assert_eq!(false,attacked_by.white[to_pos("A6".to_string()).unwrap()]);
        assert_eq!(true, attacked_by.white[to_pos("G1".to_string()).unwrap()]);
        assert_eq!(false,attacked_by.white[to_pos("H1".to_string()).unwrap()]);
        assert_eq!(true, attacked_by.white[to_pos("C3".to_string()).unwrap()]);
        assert_eq!(false,attacked_by.white[to_pos("D4".to_string()).unwrap()]);
    }
}
