open Piece
open Square
open State
open Board

(** initializes all of the pieces on the board and the state*)

(* pawn1 is white's pawn on a2 *)
let pawn1 : Piece.piece =
  {
    name = Pawn;
    color = White;
    worth = 1;
    location = ('a', 2);
    next_moves = [ ('a', 3); ('a', 4) ];
    num_moves = 0;
  }

(* pawn2 is white's pawn on b2 *)
let pawn2 : Piece.piece =
  {
    name = Pawn;
    color = White;
    worth = 1;
    location = ('b', 2);
    next_moves = [ ('b', 3); ('b', 4) ];
    num_moves = 0;
  }

(* pawn3 is white's pawn on c2 *)
let pawn3 : Piece.piece =
  {
    name = Pawn;
    color = White;
    worth = 1;
    location = ('c', 2);
    next_moves = [ ('c', 3); ('c', 4) ];
    num_moves = 0;
  }

(* pawn4  is white's pawn on d2 *)
let pawn4 : Piece.piece =
  {
    name = Pawn;
    color = White;
    worth = 1;
    location = ('d', 2);
    next_moves = [ ('d', 3); ('d', 4) ];
    num_moves = 0;
  }

(* pawn5 is white's pawn on e2 *)
let pawn5 : Piece.piece =
  {
    name = Pawn;
    color = White;
    worth = 1;
    location = ('e', 2);
    next_moves = [ ('e', 3); ('e', 4) ];
    num_moves = 0;
  }

(* pawn6 is white's pawn on f2 *)
let pawn6 : Piece.piece =
  {
    name = Pawn;
    color = White;
    worth = 1;
    location = ('f', 2);
    next_moves = [ ('f', 3); ('f', 4) ];
    num_moves = 0;
  }

(* pawn7 is white's pawn on g2 *)
let pawn7 : Piece.piece =
  {
    name = Pawn;
    color = White;
    worth = 1;
    location = ('g', 2);
    next_moves = [ ('g', 3); ('g', 4) ];
    num_moves = 0;
  }

(* pawn8 is white's pawn on h2 *)
let pawn8 : Piece.piece =
  {
    name = Pawn;
    color = White;
    worth = 1;
    location = ('h', 2);
    next_moves = [ ('h', 3); ('h', 4) ];
    num_moves = 0;
  }

(* pawn9 is black's pawn on a7 *)
let pawn9 : Piece.piece =
  {
    name = Pawn;
    color = Black;
    worth = 1;
    location = ('a', 7);
    next_moves = [ ('a', 6); ('a', 5) ];
    num_moves = 0;
  }

(* pawn10 is black's pawn on b7 *)
let pawn10 : Piece.piece =
  {
    name = Pawn;
    color = Black;
    worth = 1;
    location = ('b', 7);
    next_moves = [ ('b', 6); ('b', 5) ];
    num_moves = 0;
  }

(* pawn11 is black's pawn on c7 *)
let pawn11 : Piece.piece =
  {
    name = Pawn;
    color = Black;
    worth = 1;
    location = ('c', 7);
    next_moves = [ ('c', 6); ('c', 5) ];
    num_moves = 0;
  }

(* pawn12 is black's pawn on d7 *)
let pawn12 : Piece.piece =
  {
    name = Pawn;
    color = Black;
    worth = 1;
    location = ('d', 7);
    next_moves = [ ('d', 6); ('d', 5) ];
    num_moves = 0;
  }

(* pawn13 is black's pawn on e7 *)
let pawn13 : Piece.piece =
  {
    name = Pawn;
    color = Black;
    worth = 1;
    location = ('e', 7);
    next_moves = [ ('e', 6); ('e', 5) ];
    num_moves = 0;
  }

(* pawn14 is black's pawn on f7 *)
let pawn14 : Piece.piece =
  {
    name = Pawn;
    color = Black;
    worth = 1;
    location = ('f', 7);
    next_moves = [ ('f', 6); ('f', 5) ];
    num_moves = 0;
  }

(* pawn15 is black's pawn on g7 *)
let pawn15 : Piece.piece =
  {
    name = Pawn;
    color = Black;
    worth = 1;
    location = ('g', 7);
    next_moves = [ ('g', 6); ('g', 5) ];
    num_moves = 0;
  }

(* pawn16 is black's pawn on h7 *)
let pawn16 : Piece.piece =
  {
    name = Pawn;
    color = Black;
    worth = 1;
    location = ('h', 7);
    next_moves = [ ('h', 6); ('h', 5) ];
    num_moves = 0;
  }

(* knight1 is white's knight on b1 *)
let knight1 : Piece.piece =
  {
    name = Knight;
    color = White;
    worth = 3;
    location = ('b', 1);
    next_moves = [ ('a', 3); ('c', 3) ];
    num_moves = 0;
  }

(* knight2 is white's knight on g1 *)
let knight2 : Piece.piece =
  {
    name = Knight;
    color = White;
    worth = 3;
    location = ('g', 1);
    next_moves = [ ('f', 3); ('h', 3) ];
    num_moves = 0;
  }

(* knight3 is black's knight on b8 *)
let knight3 : Piece.piece =
  {
    name = Knight;
    color = Black;
    worth = 3;
    location = ('b', 8);
    next_moves = [ ('a', 6); ('c', 6) ];
    num_moves = 0;
  }

(* knight4 is black's knight on g8 *)
let knight4 : Piece.piece =
  {
    name = Knight;
    color = Black;
    worth = 3;
    location = ('g', 8);
    next_moves = [ ('f', 6); ('h', 6) ];
    num_moves = 0;
  }

(* bishop1 is white's bishop on c1 *)
let bishop1 : Piece.piece =
  {
    name = Bishop;
    color = White;
    worth = 3;
    location = ('c', 1);
    next_moves = [];
    num_moves = 0;
  }

(* bishop2 is white's bishop on f1 *)
let bishop2 : Piece.piece =
  {
    name = Bishop;
    color = White;
    worth = 3;
    location = ('f', 1);
    next_moves = [];
    num_moves = 0;
  }

(* bishop3 is black's bishop on c8 *)
let bishop3 : Piece.piece =
  {
    name = Bishop;
    color = Black;
    worth = 3;
    location = ('c', 8);
    next_moves = [];
    num_moves = 0;
  }

(* bishop4 is black's bishop on f8 *)
let bishop4 : Piece.piece =
  {
    name = Bishop;
    color = Black;
    worth = 3;
    location = ('f', 8);
    next_moves = [];
    num_moves = 0;
  }

(* rook1 is white's rook on a1 *)
let rook1 : Piece.piece =
  {
    name = Rook;
    color = White;
    worth = 5;
    location = ('a', 1);
    next_moves = [];
    num_moves = 0;
  }

(* rook2 is white's rook on h1 *)
let rook2 : Piece.piece =
  {
    name = Rook;
    color = White;
    worth = 5;
    location = ('h', 1);
    next_moves = [];
    num_moves = 0;
  }

(* rook3 is black's rook on a8 *)
let rook3 : Piece.piece =
  {
    name = Rook;
    color = Black;
    worth = 5;
    location = ('a', 8);
    next_moves = [];
    num_moves = 0;
  }

(* rook4 is black's rook on h8 *)
let rook4 : Piece.piece =
  {
    name = Rook;
    color = Black;
    worth = 5;
    location = ('h', 8);
    next_moves = [];
    num_moves = 0;
  }

(* queen1 is white's queen on d1 *)
let queen1 : Piece.piece =
  {
    name = Queen;
    color = White;
    worth = 9;
    location = ('d', 1);
    next_moves = [];
    num_moves = 0;
  }

(* queen2 is black's queen on d8 *)
let queen2 : Piece.piece =
  {
    name = Queen;
    color = Black;
    worth = 9;
    location = ('d', 8);
    next_moves = [];
    num_moves = 0;
  }

(* king1 is white's king on e1 *)
let king1 : Piece.piece =
  {
    name = King;
    color = White;
    worth = 9000;
    location = ('e', 1);
    next_moves = [];
    num_moves = 0;
  }

(* king2 is black's king on e8 *)
let king2 : Piece.piece =
  {
    name = King;
    color = Black;
    worth = 9000;
    location = ('e', 8);
    next_moves = [];
    num_moves = 0;
  }

(* piecelist is the initial piecelist to begin the game *)
let piecelist =
  [
    pawn1;
    pawn2;
    pawn3;
    pawn4;
    pawn5;
    pawn6;
    pawn7;
    pawn8;
    pawn9;
    pawn10;
    pawn11;
    pawn12;
    pawn13;
    pawn14;
    pawn15;
    pawn16;
    knight1;
    knight2;
    knight3;
    knight4;
    bishop1;
    bishop2;
    bishop3;
    bishop4;
    rook1;
    rook2;
    rook3;
    rook4;
    queen1;
    queen2;
    king1;
    king2;
  ]

(** Defines all of the squares on a board *)

(*Rank 1*)
let square_a1 : Square.square =
  { file = 'a'; rank = 1; occupant = Some rook1; color = Black }

let square_b1 : Square.square =
  { file = 'b'; rank = 1; occupant = Some knight1; color = White }

let square_c1 : Square.square =
  { file = 'c'; rank = 1; occupant = Some bishop1; color = Black }

let square_d1 : Square.square =
  { file = 'd'; rank = 1; occupant = Some queen1; color = White }

let square_e1 : Square.square =
  { file = 'e'; rank = 1; occupant = Some king1; color = Black }

let square_f1 : Square.square =
  { file = 'f'; rank = 1; occupant = Some bishop2; color = White }

let square_g1 : Square.square =
  { file = 'g'; rank = 1; occupant = Some knight2; color = Black }

let square_h1 : Square.square =
  { file = 'h'; rank = 1; occupant = Some rook2; color = White }

(*Rank 2*)

let square_a2 : Square.square =
  { file = 'a'; rank = 2; occupant = Some pawn1; color = White }

let square_b2 : Square.square =
  { file = 'b'; rank = 2; occupant = Some pawn2; color = Black }

let square_c2 : Square.square =
  { file = 'c'; rank = 2; occupant = Some pawn3; color = White }

let square_d2 : Square.square =
  { file = 'd'; rank = 2; occupant = Some pawn4; color = Black }

let square_e2 : Square.square =
  { file = 'e'; rank = 2; occupant = Some pawn5; color = White }

let square_f2 : Square.square =
  { file = 'f'; rank = 2; occupant = Some pawn6; color = Black }

let square_g2 : Square.square =
  { file = 'g'; rank = 2; occupant = Some pawn7; color = White }

let square_h2 : Square.square =
  { file = 'h'; rank = 2; occupant = Some pawn8; color = Black }

(*Rank 3*)

let square_a3 : Square.square =
  { file = 'a'; rank = 3; occupant = None; color = Black }

let square_b3 : Square.square =
  { file = 'b'; rank = 3; occupant = None; color = White }

let square_c3 : Square.square =
  { file = 'c'; rank = 3; occupant = None; color = Black }

let square_d3 : Square.square =
  { file = 'd'; rank = 3; occupant = None; color = White }

let square_e3 : Square.square =
  { file = 'e'; rank = 3; occupant = None; color = Black }

let square_f3 : Square.square =
  { file = 'f'; rank = 3; occupant = None; color = White }

let square_g3 : Square.square =
  { file = 'g'; rank = 3; occupant = None; color = Black }

let square_h3 : Square.square =
  { file = 'h'; rank = 3; occupant = None; color = White }

(*Rank 4*)

let square_a4 : Square.square =
  { file = 'a'; rank = 4; occupant = None; color = White }

let square_b4 : Square.square =
  { file = 'b'; rank = 4; occupant = None; color = Black }

let square_c4 : Square.square =
  { file = 'c'; rank = 4; occupant = None; color = White }

let square_d4 : Square.square =
  { file = 'd'; rank = 4; occupant = None; color = Black }

let square_e4 : Square.square =
  { file = 'e'; rank = 4; occupant = None; color = White }

let square_f4 : Square.square =
  { file = 'f'; rank = 4; occupant = None; color = Black }

let square_g4 : Square.square =
  { file = 'g'; rank = 4; occupant = None; color = White }

let square_h4 : Square.square =
  { file = 'h'; rank = 4; occupant = None; color = Black }

(*Rank 5*)

let square_a5 : Square.square =
  { file = 'a'; rank = 5; occupant = None; color = Black }

let square_b5 : Square.square =
  { file = 'b'; rank = 5; occupant = None; color = White }

let square_c5 : Square.square =
  { file = 'c'; rank = 5; occupant = None; color = Black }

let square_d5 : Square.square =
  { file = 'd'; rank = 5; occupant = None; color = White }

let square_e5 : Square.square =
  { file = 'e'; rank = 5; occupant = None; color = Black }

let square_f5 : Square.square =
  { file = 'f'; rank = 5; occupant = None; color = White }

let square_g5 : Square.square =
  { file = 'g'; rank = 5; occupant = None; color = Black }

let square_h5 : Square.square =
  { file = 'h'; rank = 5; occupant = None; color = White }

(*Rank 6*)

let square_a6 : Square.square =
  { file = 'a'; rank = 6; occupant = None; color = White }

let square_b6 : Square.square =
  { file = 'b'; rank = 6; occupant = None; color = Black }

let square_c6 : Square.square =
  { file = 'c'; rank = 6; occupant = None; color = White }

let square_d6 : Square.square =
  { file = 'd'; rank = 6; occupant = None; color = Black }

let square_e6 : Square.square =
  { file = 'e'; rank = 6; occupant = None; color = White }

let square_f6 : Square.square =
  { file = 'f'; rank = 6; occupant = None; color = Black }

let square_g6 : Square.square =
  { file = 'g'; rank = 6; occupant = None; color = White }

let square_h6 : Square.square =
  { file = 'h'; rank = 6; occupant = None; color = Black }

(*Rank 7*)

let square_a7 : Square.square =
  { file = 'a'; rank = 7; occupant = Some pawn9; color = Black }

let square_b7 : Square.square =
  { file = 'b'; rank = 7; occupant = Some pawn10; color = White }

let square_c7 : Square.square =
  { file = 'c'; rank = 7; occupant = Some pawn11; color = Black }

let square_d7 : Square.square =
  { file = 'd'; rank = 7; occupant = Some pawn12; color = White }

let square_e7 : Square.square =
  { file = 'e'; rank = 7; occupant = Some pawn13; color = Black }

let square_f7 : Square.square =
  { file = 'f'; rank = 7; occupant = Some pawn14; color = White }

let square_g7 : Square.square =
  { file = 'g'; rank = 7; occupant = Some pawn15; color = Black }

let square_h7 : Square.square =
  { file = 'h'; rank = 7; occupant = Some pawn16; color = White }

(*Rank 8*)
let square_a8 : Square.square =
  { file = 'a'; rank = 8; occupant = Some rook3; color = White }

let square_b8 : Square.square =
  { file = 'b'; rank = 8; occupant = Some knight3; color = Black }

let square_c8 : Square.square =
  { file = 'c'; rank = 8; occupant = Some bishop3; color = White }

let square_d8 : Square.square =
  { file = 'd'; rank = 8; occupant = Some queen2; color = Black }

let square_e8 : Square.square =
  { file = 'e'; rank = 8; occupant = Some king2; color = White }

let square_f8 : Square.square =
  { file = 'f'; rank = 8; occupant = Some bishop4; color = Black }

let square_g8 : Square.square =
  { file = 'g'; rank = 8; occupant = Some knight4; color = White }

let square_h8 : Square.square =
  { file = 'h'; rank = 8; occupant = Some rook4; color = Black }

(* Defines rank1 *)

(** Initializes all of the ranks with the proper squares using a square list *)
let rank1 =
  [|
    square_a1;
    square_b1;
    square_c1;
    square_d1;
    square_e1;
    square_f1;
    square_g1;
    square_h1;
  |]

(* Defines rank2 *)
let rank2 =
  [|
    square_a2;
    square_b2;
    square_c2;
    square_d2;
    square_e2;
    square_f2;
    square_g2;
    square_h2;
  |]

(* Defines rank3 *)
let rank3 =
  [|
    square_a3;
    square_b3;
    square_c3;
    square_d3;
    square_e3;
    square_f3;
    square_g3;
    square_h3;
  |]

(* Defines rank4 *)
let rank4 =
  [|
    square_a4;
    square_b4;
    square_c4;
    square_d4;
    square_e4;
    square_f4;
    square_g4;
    square_h4;
  |]

(* Defines rank5 *)
let rank5 =
  [|
    square_a5;
    square_b5;
    square_c5;
    square_d5;
    square_e5;
    square_f5;
    square_g5;
    square_h5;
  |]

(* Defines rank6 *)
let rank6 =
  [|
    square_a6;
    square_b6;
    square_c6;
    square_d6;
    square_e6;
    square_f6;
    square_g6;
    square_h6;
  |]

(* Defines rank7 *)
let rank7 =
  [|
    square_a7;
    square_b7;
    square_c7;
    square_d7;
    square_e7;
    square_f7;
    square_g7;
    square_h7;
  |]

(* Defines rank8 *)
let rank8 =
  [|
    square_a8;
    square_b8;
    square_c8;
    square_d8;
    square_e8;
    square_f8;
    square_g8;
    square_h8;
  |]

(* Defines the board *)
let board = [| rank8; rank7; rank6; rank5; rank4; rank3; rank2; rank1 |]

(* Defines the initial state of the game *)
let state : State.state =
  {
    turn = White;
    inCheckwhite = false;
    inCheckblack = false;
    stalematewhite = false;
    stalemateblack = false;
    checkmatewhite = false;
    checkmateblack = false;
    white_has_captured = [| 0; 0; 0; 0; 0 |];
    black_has_captured = [| 0; 0; 0; 0; 0 |];
  }
